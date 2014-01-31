#ifndef IMGPROC_NIF_HPP
#define IMGPROC_NIF_HPP

#include <opencv2/core/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <opencv2/calib3d/calib3d.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <opencv/highgui.h>
#include <opencv/cv.h>

#include <iostream>
#include <sstream>
#include <sys/time.h>
#include <stdio.h>
#include <math.h>

using namespace cv;
using namespace std;

//#################################### DEFINES #####################################
#define LINECOVERAGE 220.0

#define CENTER_X 376
#define CENTER_Y 240

#define LEFT_LINE 3
#define DASHED_LINE 2
#define RIGHT_LINE 1

#define LEFT_MIN 327.0
#define LEFT_MAX 359.0
#define DASHED_MIN 250.0
#define DASHED_MAX 320.0
#define RIGHT_MIN 210.0
#define RIGHT_MAX 238.0

#define PI 3.141592653589793238462

//#################################### STRUCTS #####################################
typedef struct _range {
  int left;
  int mid;
  int right;
  int length;
} range;

typedef struct _frame_t {
  IplImage* _frame;
} frame_t;

//############################## FUNCTION DEFINITIONS ##############################
float dist(CvPoint p1, CvPoint p2);
float dist(Point2f p1, Point2f p2);
bool is_trash(vector<vector<Point2i> > line);
float get_angle( Point2f a, Point2f b, Point2f c );
Point2f center_point(Point2f a, Point2f b);
int find_dashed(vector<vector<vector<vector<Point2i> > > > grouped, Point2f intersection_points1, Point2f intersection_points2);
void find_intersection(vector<vector<vector<Point2i> > > lanes, Point2f* p1, Point2f* p2);
bool find_stop_line(vector<vector<Point2i> > line);

//############################## FUNCTION DECLARATIONS #############################
float dist(CvPoint p1, CvPoint p2) {
  return sqrt((p2.y - p1.y) * (p2.y - p1.y) + (p2.x - p1.x) * (p2.x - p1.x));
}

float dist(Point2f p1, Point2f p2) {
  return sqrt((p2.y - p1.y) * (p2.y - p1.y) + (p2.x - p1.x) * (p2.x - p1.x));
}

bool is_trash(vector<vector<Point2i> > line){

  if(line.size() < 5)
    return true;

  for (unsigned int i = 0; i < line.size(); ++i) {
    //float length = (((line[i][1].y  - LINECOVERAGE) / (480.0 - LINECOVERAGE)) * 30.0) + 15;
    float length = (((line[i][1].y - LINECOVERAGE) / (480.0 - LINECOVERAGE)) * 30.0) + 15; 
    unsigned int width = line[i][1].x - line[i][0].x;
    if(width > length ){
      //cout << "WIDTH : " << width << " LENGTH "<< length<<  " HOW MUCH LEFT? "<< line.size() - i << "  I  "<<  i <<endl;
      if(line.size() > i + 5 ){
	//cout << "REMOVED" << endl;
	return true;
      }
    }
    if(width > line.size())
      return true;
  }

  return false;
}

float get_angle( Point2f a, Point2f b, Point2f c ){
  Point2f ab = Point2f(b.x - a.x, b.y - a.y );
  Point2f cb = Point2f(b.x - c.x, b.y - c.y );

  float dot = (ab.x * cb.x + ab.y * cb.y); // dot product
  float cross = (ab.x * cb.y - ab.y * cb.x); // cross product

  float alpha = atan2(cross, dot);

  return alpha * 180 / PI;
}


Point2f center_point(Point2f a, Point2f b){
  return Point2f((a.x+b.x)/2 , (a.y+b.y)/2 );
}

int find_dashed(vector<vector<vector<vector<Point2i> > > > grouped, Point2f *inter_angle, Point2f* inter_angle_before) {
  
  Point2f start;
  Point2f start_top;
  Point2f end;

  for (unsigned int i = 0; i < grouped.size(); i++) {
    if (grouped[i].size() > 1) {
      bool correct = true;
      for (unsigned int j = 0; j < grouped[i].size() - 1; j++) {
	int last = grouped[i][j].size() - 1;
	start = center_point(grouped[i][j][last][0],
				     grouped[i][j][last][1]);

	start_top = center_point(grouped[i][j][0][0],
					 grouped[i][j][0][1]);

	float dash_length = dist(start, start_top);

	end = center_point(grouped[i][j + 1][0][0], grouped[i][j + 1][0][1]);

	float distance = start.y - end.y; // dist(start, end);
	float length = (end.y - LINECOVERAGE ) / 90 * 70 + 15; //8888888

	if(((end.y - inter_angle.y) < 25) && (end.y - inter_angle.y > 0) && 
	   abs(inter_angle.x - end.x < 130))
	  {
	  }
	else{
	  *inter_angle = Point2f(0,0);
	  *inter_angle_before = Point2f(0,0);
	}
	if (distance > length || dash_length > 100) {
	  correct = false;
	}
      }
      if (correct) {
	return i;
      }
    }
  }
  return -1;
}

Point2f find_stop_line(vector<vector<Point2i> > line)
{
  int last = line.size()-1;
  
  if((line[0][1].x > 280 && line[last][1].x < 360) && (line[0][0].y > 249 && line[last][0].y < 340) && (line[0][1].y > 249 && line[last][1].y < 340))
    {
      if(((line[0][1].x - line[0][0].x) > 15) && (abs(line[last][0].y - line[0][0].y) < 10))
	{
	  Point2f stop_line = center_point(line[0][1], line[0][0])
	  return stop_line;
	}
    }
  return Point2f(0,0);
}

void find_intersection(vector<vector<vector<Point2i> > > lanes, Point2f* p1, Point2f* p2) {
  for (unsigned int i = 0; i < lanes.size(); i++) {
    if (lanes[i].size() > 11) {
      for (unsigned int j = 5; j < lanes[i].size() - 6; j++) {
	Point2f before_start;
	Point2f start;
	Point2f center;
	Point2f end;
	if(j > 15){
	  before_start.x = (lanes[i][j-15][0].x + lanes[i][j-15][1].x) / 2;
	  before_start.y = lanes[i][j-15][0].y;
	}
	start.x = (lanes[i][j - 5][0].x + lanes[i][j - 5][1].x) / 2;
	start.y = lanes[i][j - 5][0].y;
	
	center.x = (lanes[i][j][0].x + lanes[i][j][1].x) / 2;
	center.y = lanes[i][j][0].y;

	end.x = (lanes[i][j + 5][0].x + lanes[i][j + 5][1].x) / 2;
	end.y = lanes[i][j + 5][0].y;
	
	if(end.y < 240 || end.x < 200 || end.x > 450)
	  continue; 

	float alpha = get_angle(end, center, start);

	if (((alpha > 80) && (alpha < 100))) {
	  *p1 = start;
	  *p2 = before_start;
	  break;
	} 
	*p1 = Point2f(20000,20000);
	*p2 = Point2f(10000,10000);
      }
      break;
    }  
  }
}


#endif /* IMGPROC_NIF_HPP */
