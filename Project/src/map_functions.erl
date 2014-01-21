-module(map_functions).

-include("../include/offsetCalculation.hrl").

-record(dash_line, {center_point, box, points, area, dash_before, dash_after}).

-compile(export_all).


translate_points(ID, [Point | T] , Buff) ->
    case translate_point(ID, Point) of
	[] ->
	    translate_points(ID, T, Buff);
	NewPoint ->
	    translate_points(ID, T, Buff ++ [NewPoint])
    end;
translate_points(_,[], Buff) ->
    Buff.

translate_dash(ID, {CenterPoint, {{R1,R2,R3,R4} , {Bottom, Middle, Top}}}, {Car_Pos, Car_Heading}) ->
    Center = steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID, CenterPoint)),
    BottomLeft =  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R1)),
    TopLeft =  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R2)),
    TopRight =  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R3)),
    BottomRight =  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,R4)),
    #dash_line{center_point = Center, 
	       box = {BottomLeft, TopLeft, TopRight, BottomRight},
	       points = [ steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Bottom)),
			  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Middle)),
			  steering:local_to_global(Car_Pos, Car_Heading, translate_point(ID,Top))], 
	       area = calculate_box_area(BottomLeft, TopLeft, TopRight, BottomRight),
	       dash_before = undef, dash_after = undef}.

translate_dashes(ID, [Dash | T], {Car_Pos,Car_Heading} , Buff) ->
    translate_dashes(ID, T, {Car_Pos,Car_Heading}, 
		     Buff ++ [ translate_dash(ID, Dash, {Car_Pos,Car_Heading}) ]); 
translate_dashes(_,[],_,Buff) ->
    Buff.

translate_point(ID, Point) ->
    case ets:lookup(ID , Point) of
	[] ->
	    {200000000, 200000000};
	[{_,NewPoint}] ->
	    NewPoint
    end.


calculate_offsets(InputLane, OutputType, [Dash | T] , Buff) ->
    {ok,[P]} = offsetCalculation:calculate_offset_list(InputLane, OutputType, Dash#dash_line.points),
    calculate_offsets(InputLane, OutputType, T , Buff ++ [P]);
calculate_offsets(_, _, [] , Buff) ->
    Buff.

read_cm_file(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    case io:get_line(Device, "") of
        eof  -> file:close(Device);
        Line ->	T = lists:map(fun(X) -> list_to_float(X) end, string:tokens(Line -- "\n" , ",")),
		list_to_tuple(T)
    end.


center_point({X1,Y1}, {X2,Y2}) ->
    {(X1 +X2)/2, (Y1+Y2)/2}.

offset_point({X1,Y1}, {X2,Y2}) ->
    {X2-X1, Y2-Y1}.

closest_in_radius(Dash, [Last | T], {Prev_Distance, Closest_Dash}) ->
    Distance = getDistance(Dash, Last),  
    case Distance < Prev_Distance of
	true ->
	    closest_in_radius(Dash, T, {Distance, Last});    
	false ->
	    closest_in_radius(Dash, T, {Prev_Distance, Closest_Dash})
    end;      
closest_in_radius(_, [] , {_, undef}) ->
    not_found;
closest_in_radius(_, [] , {_, Dash}) ->
    ets_lookup(Dash).


find_closest_dash_ahead({Cx,Cy}, Radius) ->
    MatchSpec = match_spec(Cx,Cy,Radius),
    T = ets:select(dash_lines, MatchSpec),
    case T of
	[] ->
	    not_found;
	_ ->
     	    element(2,hd(ets:lookup(dash_lines, element(2,lists:min(T)))))
    end.

rect_angle({{X1,Y1},_,{X2,Y2},_}) ->
    math:atan2(Y2-Y1 , X2-X1).


calculate_correct_pos([Dash| T], Last_Dashes) ->
    case closest_in_radius(Dash#dash_line.center_point, Last_Dashes,  {300, undef}) of
	not_found ->
	    calculate_correct_pos(T, Last_Dashes);
	Corresponding_Dash ->
	    Offset = offset_point(Dash#dash_line.center_point , 
				  Corresponding_Dash#dash_line.center_point),
	    Angle1 = rect_angle(Dash#dash_line.box),
	    Angle2 = rect_angle(Corresponding_Dash#dash_line.box),
	    Delta_Angle = Angle2 - Angle1, %%steering:normalized((Angle2 - Angle1)),
	    Length = get_length(Dash#dash_line.points),
	    case {Length > 185 , Length < 250 } of
		{true, true} ->
		    {[Dash|T] , Corresponding_Dash, {Offset, Delta_Angle}};
		_ ->
		    calculate_correct_pos(T, Last_Dashes)
	    end
    end;
calculate_correct_pos([], _ ) ->
    not_found.
    
calculate_estimated_correction([Dash| T] , Estimated_Dashes) ->
    case find_closest_dash(Dash#dash_line.center_point, Estimated_Dashes, {400, undef}) of
	not_found ->
	    calculate_estimated_correction(T, Estimated_Dashes);
	Corresponding_Dash ->
	    Offset = offset_point(Dash#dash_line.center_point , Corresponding_Dash#dash_line.center_point),
	    Angle1 = rect_angle(Dash#dash_line.box),
	    Angle2 = rect_angle(Corresponding_Dash#dash_line.box),
	    Delta_Angle = Angle2 -  Angle1, %% steering:normalized((Angle2 - Angle1)),
	    Length = get_length(Dash#dash_line.points),
	    case {Length > 190 , Length < 250 %%,
		 %% Delta_Angle < (math:pi() / 180 * 25), 
		 %% Delta_Angle > (math:pi() / 180 * -25)
		 } of
		{true, true} -> %%, true , true} ->
		    {[Dash| T] , Corresponding_Dash, {Offset, Delta_Angle}};
		_ ->
		    calculate_estimated_correction(T, Estimated_Dashes)
	    end
    end;
calculate_estimated_correction([], _) ->
    not_found.


find_closest_dash(Dash, [Estimated_Dash| T], {Prev_Distance, Closest_Dash}) ->
    Distance = getDistance(Dash, Estimated_Dash#dash_line.center_point),
    case Distance < Prev_Distance of
	true ->
	    find_closest_dash(Dash, T, {Distance, Estimated_Dash});    
	false ->
	    find_closest_dash(Dash, T, {Prev_Distance, Closest_Dash})
    end;    
find_closest_dash(_, [] , {_, undef}) ->
    not_found;
find_closest_dash(_, [] , {_, Dash}) ->
    Dash.

calculate_box_area({X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4}) ->
    abs((X1*Y2 - X2*Y1 + X2*Y3 - X3*Y2 + X3*Y4 - X4*Y3 + X4*Y1 - X1*Y4) / 2).


move_dashes([Dash|T], Correction, Buff) ->
    move_dashes(T, Correction, Buff ++ [move_dash(Dash,Correction)]);
move_dashes([],_,Buff) ->
    Buff.

%%    Dash = ets_lookup(Point),
%%    New_Dash = move_dash(Dash, Correction),
%%    ets:insert(dash_lines, {Point, New_Dash}),
%%    case Dash#dash_line.dash_after of
%%	undef ->
%%	    ok;
%%	After ->
%%	    move_dashes(After, Correction)
%%    end.

move_dash(#dash_line{center_point = Center, 
		     box = {P1,P2,P3,P4}, 
		     points = [BottomP, CenterP, TopP]} , Correction) ->
    

    #dash_line{center_point = move_point(Center, Correction) ,
	       box = {move_point(P1, Correction),
		      move_point(P2, Correction),
		      move_point(P3, Correction),
		      move_point(P4, Correction)},
	       points = [move_point(BottomP, Correction),
			 move_point(CenterP, Correction),
			 move_point(TopP, Correction)],
	       area = calculate_box_area(move_point(P1, Correction),
					 move_point(P2, Correction),
					 move_point(P3, Correction),
					 move_point(P4, Correction))}.
	
					 
move_point({X,Y},{{Cx,Cy},{{Dx,Dy}, Rotation}}) ->
    
    Distance = getDistance({X,Y}, {Cx,Cy}),
    Angle = getAng({Cx,Cy}, {X,Y}),
    NewX = (Distance * math:cos(Angle + Rotation)) + Cx + Dx,
    
    NewY = (Distance * math:sin(Angle + Rotation)) + Cy + Dy,

    {round(X+Dx), round(Y+Dy)}.
%%    {round(NewX), round(NewY)}.

local_to_global({Cx,Cy} , Rotation, {X,Y}) ->
    
    Distance = getDistance({X,Y}, {Cx,Cy}),
    Angle = getAng({X,Y}, {Cx,Cy}),
    NewX = (Distance * math:cos(Angle+Rotation)) + Cx ,
    
    NewY = (Distance * math:sin(Angle+Rotation)) + Cy ,

    {round(NewX), round(NewY)}.


ets_lookup(Point) ->
    case ets:lookup(dash_lines, Point) of
	[] ->
	    ok;
	[{Point, Value}] ->
	    Value
    end.



getAng({X1,Y1} , {X2,Y2}) -> 
    math:atan2(Y2-Y1,X2-X1).
					
getDistance({X1,Y1} , {X2,Y2}) ->
    math:sqrt(math:pow(Y2-Y1,2) + math:pow(X2-X1,2)).
   


connect_dashes([H1,H2|T] , Before , Buff) ->
    connect_dashes([H2|T], H1#dash_line.center_point , 
		   Buff ++ [H1#dash_line{dash_before = Before , 
					 dash_after = H2#dash_line.center_point}]);
connect_dashes([H], Before, Buff) ->
    Buff ++ [H#dash_line{dash_before = Before , 
			 dash_after = undef}];
connect_dashes(_,_,_) ->
    [].


remove_dashes_before({Cx,Cy},List = [H|T]) ->
    case H#dash_line.center_point of
	{Cx,Cy} ->
	    List;
	_ ->
	    remove_dashes_before({Cx,Cy}, T)
    end;
remove_dashes_before(_,[]) ->
    well_fuck.

clean_ets_dashes(Point) ->
    case ets_lookup(Point) of 
	ok -> 
	    ok;
	Orig_Dash -> 
	    ets:delete(dash_lines, Point),
	    case Orig_Dash#dash_line.dash_after of
		undef ->
		    ok;
		Next ->
		    clean_ets_dashes(Next)
	    end
    end.


insert_dashes([Dash|T]) ->
    ets:insert(dash_lines, {Dash#dash_line.center_point, Dash}),
    insert_dashes(T);
insert_dashes([]) ->
    ok.

match_spec(CX,CY,R) ->
[{{{'$1','$2'},'$3'},
  [{'andalso',{'<',{'+',{'*',{'-','$1',{const,CX}},
                             {'-','$1',{const,CX}}},
                        {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
                   {'*',{const,R},{const,R}}},
              {'>',{'+',{'*',{'-','$1',{const,CX}},{'-','$1',{const,CX}}},
                        {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
                   {'-',{'*',{const,R},{const,R}}}}}],
  [{{{'+',{'*',{'-','$1',{const,CX}},{'-','$1',{const,CX}}},
          {'*',{'-','$2',{const,CY}},{'-','$2',{const,CY}}}},
     {{'$1','$2'}}}}]}].

		  
get_length([P1,P2,P3]) ->
    steering:getDistance(P1,P3).


separate([{_,H} | T] , Buff, 0) ->
    {ok,[OP2]} = offsetCalculation:calculate_offset_list(2, 1, H#dash_line.points),   
    separate(T, Buff ++ H#dash_line.points ++ [OP2], 0);
separate([{_,H} | T] , Buff, 1) ->
    separate(T, Buff ++ lists:nth(2, H#dash_line.points) , 1);
separate([{_,H} | T] , Buff, 2) ->
    separate(T, Buff ++ H#dash_line.points, 2);
separate([], Buff, _) ->
    Buff.


get_last_dashes() ->
    Match_Spec = [{{'$1',{'_','_','_','_','_','_','$2'}},
		   [{'==','$2', undef}],
		   ['$1']}],
    case ets:select(dash_lines, Match_Spec) of
	[] ->
	    [];
	[Last] ->
	    get_last_dashes(5, Last)
    end.

get_last_dashes(0, _) ->
    [];
get_last_dashes(N, Last) ->
    case ets:lookup(dash_lines, Last) of
	[] ->
	    [];
	[{K,V}] ->
	    case V#dash_line.dash_before of
		undef ->
		    [K];
		L ->
		    get_last_dashes(N-1, L) ++ [K]
	    end
    end.


get_dashes_ahead({CarX, CarY}, Heading) ->
    Dashes = ets:select(dash_lines, map_functions:match_spec(CarX, CarY, 600)),
    get_dashes_ahead(Dashes , {{CarX, CarY}, Heading});
get_dashes_ahead([], _) ->
    io:format("FUCK MY LIFE NOTHING IS IN FRONT~n", []);
get_dashes_ahead([{_, Dash} | T], {Position, Heading}) ->
    Angle = (Heading - steering:getAng(Position, Dash)) * 180 / math:pi(),
    case {Angle < 90, Angle > -90} of
	{true, true} ->
	    get_dashes_ahead(5, Dash);
	_ ->
	    get_dashes_ahead(T, {Position, Heading})
    end;
get_dashes_ahead(0, _) ->
    [];
get_dashes_ahead(N, Dash) ->
     case ets:lookup(dash_lines, Dash) of
	[] ->
	    [];
	[{K,V}] ->
	    case V#dash_line.dash_after of
		undef ->
		    [K];
		L ->
		    [K| get_dashes_ahead(N-1, L)]
	    end
     end.





    %% case length(List) > 5 of
    %% 	true ->
    %% 	    lists:sublist(List, length(List) -4 , 5);
    %% 	_ ->
    %% 	    List
    %% end.




add_dashes({X1,Y1}, {X2,Y2}, {X3,Y3},Amount)->
    ValX = min(abs(X1-X2)*1000000, abs(X2-X3)*1000000),
    ValY = min(abs(Y1-Y2)*1000000, abs(Y2-Y3)*1000000),
    B1 = (ValX < 1) or (ValY < 1),
    B2 = math:atan2(Y2-Y1,X2-X1) == math:atan2(Y3-Y2,X3-X2),
    case  {B1 , B2} of 
        {false, false} ->
            case steering:findcircle({X1,Y1}, {X2,Y2}, {X3,Y3}) of 
                infinite -> 
		    Points = gen_dash_straight({{X2,Y2}, {X3,Y3}}, Amount);
                {CenterPoint, Radius, Clockwise} -> 
                    Points = gen_dash_circle( {CenterPoint, Radius, Clockwise}, {X3,Y3}, Amount);
                Unknown ->
                    Points = gen_dash_straight({{X2,Y2}, {X3,Y3}}, Amount)       
            end;
        _ ->
            Points = gen_dash_straight({{X2,Y2}, {X3,Y3}}, Amount)
    end,
    Points.

gen_dash_straight({P2, P3}, Amount) ->
    Angel = getAng(P2, P3),
    gen_dash_straight([], Amount, Angel, P3).
gen_dash_straight(Points, 0, _Angel, _LastPoint) ->
    Points;
gen_dash_straight(Points, Amount, Angel, {XPoint,YPoint}) ->
    Xc = XPoint+200*Amount*math:cos(Angel),
    Yc = YPoint+200*Amount*math:sin(Angel),
    Xbl = XPoint+200*Amount*math:cos(Angel)-100*math:cos(Angel)-10*math:cos(Angel-(math:pi()/2)),
    Ybl = YPoint+200*Amount*math:sin(Angel)-100*math:sin(Angel)-10*math:sin(Angel-(math:pi()/2)),
    Xtl = XPoint+200*Amount*math:cos(Angel)+100*math:cos(Angel)-10*math:cos(Angel-(math:pi()/2)),
    Ytl = YPoint+200*Amount*math:sin(Angel)+100*math:sin(Angel)-10*math:sin(Angel-(math:pi()/2)),
    Xtr = XPoint+200*Amount*math:cos(Angel)+100*math:cos(Angel)+10*math:cos(Angel-(math:pi()/2)),
    Ytr = YPoint+200*Amount*math:sin(Angel)+100*math:sin(Angel)+10*math:sin(Angel-(math:pi()/2)),
    Xbr = XPoint+200*Amount*math:cos(Angel)-100*math:cos(Angel)+10*math:cos(Angel-(math:pi()/2)),
    Ybr = YPoint+200*Amount*math:sin(Angel)-100*math:sin(Angel)+10*math:sin(Angel-(math:pi()/2)),
    Xcb = XPoint+200*Amount*math:cos(Angel)-100*math:cos(Angel),
    Ycb = YPoint+200*Amount*math:sin(Angel)-100*math:sin(Angel),
    Xct = XPoint+200*Amount*math:cos(Angel)+100*math:cos(Angel),
    Yct = YPoint+200*Amount*math:sin(Angel)+100*math:sin(Angel),
    Dash = #dash_line{center_point = {Xc,Yc} , 
		      box= {{Xbl,Ybl},{Xtl,Ytl},{Xtr,Ytr},{Xbr,Ybr}},
		      points = [{Xcb,Ycb},{Xc,Yc},{Xct,Yct}]}, 
    gen_dash_straight([Dash| Points], Amount-1, Angel, {XPoint,YPoint}).

 
gen_dash_circle({{CenterX,CenterY},Radius,ClockWise}, {DashX,DashY}, Amount)->
    Dash2Angel = math:atan2(DashY-CenterY,DashX-CenterX),
    gen_dash_circle([], Amount, Dash2Angel, {CenterX,CenterY},Radius,ClockWise).

gen_dash_circle(Points, 0, _LastPointAngel, {_CenterX,_CenterY},_Radius,_ClockWise)->
    Points;
gen_dash_circle(Points, Amount, LastPointAngel, {CenterX,CenterY},Radius,ClockWise)->
    Xc = CenterX+Radius*math:cos(LastPointAngel-Amount*(200*ClockWise/Radius)),
    Yc = CenterY+Radius*math:sin(LastPointAngel-Amount*(200*ClockWise/Radius)),
    Xbl = CenterX+(Radius+(10*ClockWise))*
	math:cos(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Ybl = CenterY+(Radius+(10*ClockWise))*
	math:sin(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Xtl = CenterX+(Radius+(10*ClockWise))*
	math:cos(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Ytl = CenterY+(Radius+(10*ClockWise))*
	math:sin(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Xtr = CenterX+(Radius-(10*ClockWise))*
	math:cos(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Ytr = CenterY+(Radius-(10*ClockWise))*
	math:sin(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Xbr = CenterX+(Radius-(10*ClockWise))*
	math:cos(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Ybr = CenterY+(Radius-(10*ClockWise))*
	math:sin(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Xcb = CenterX+(Radius)*
	math:cos(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Ycb = CenterY+(Radius)*
	math:sin(LastPointAngel+(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Xct = CenterX+(Radius)*
	math:cos(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Yct = CenterY+(Radius)*
	math:sin(LastPointAngel-(100*ClockWise/Radius)-Amount*(200*ClockWise/Radius)),
    Dash = #dash_line{center_point = {Xc,Yc},
		      box = {{Xbl,Ybl},{Xtl,Ytl},{Xtr,Ytr},{Xbr,Ybr}},
		      points = [{Xcb,Ycb},{Xc,Yc},{Xct,Yct}]},
    gen_dash_circle([Dash| Points], Amount-1, LastPointAngel, {CenterX,CenterY},Radius,ClockWise).



get_last([Last]) ->
    ets_lookup(Last);
get_last([_|T]) ->
    get_last(T).


generate_estimated_dashes([_,_,P1,P2,P3]) ->
    add_dashes(P1,P2,P3,8);
generate_estimated_dashes([_,P1,P2,P3]) ->
    add_dashes(P1,P2,P3,8);
generate_estimated_dashes([P1,P2,P3]) ->
    add_dashes(P1,P2,P3,8);
generate_estimated_dashes(_) ->
    [].



get_nodes_ahead([Head| T], Buff) ->
    Dash = ets_lookup(Head),
    {ok,[OP]} = offsetCalculation:calculate_offset_list(?InputLaneD, ?LaneAdjacent, 
							Dash#dash_line.points),   
    get_nodes_ahead(T, Buff ++ [OP]);
get_nodes_ahead([], Buff) ->
    Buff.
