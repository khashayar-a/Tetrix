/*******************************************************
 HIDAPI - Multi-Platform library for
 communication with HID devices.

 Alan Ott
 Signal 11 Software

 8/22/2009
 Linux Version - 6/2/2009

 Copyright 2009, All Rights Reserved.
 
 At the discretion of the user of this library,
 this software may be licensed under the terms of the
 GNU Public License v3, a BSD-Style license, or the
 original HIDAPI license as outlined in the LICENSE.txt,
 LICENSE-gpl3.txt, LICENSE-bsd.txt, and LICENSE-orig.txt
 files located at the root of the source distribution.
 These files may also be found in the public source
 code repository located at:
        http://github.com/signal11/hidapi .
********************************************************/

/* C */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <locale.h>
#include <errno.h>

/* Unix */
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <poll.h>

/* Linux */
#include <linux/hidraw.h>
#include <linux/version.h>
#include <libudev.h>

#include "hidapi.h"

/* Definitions from linux/hidraw.h. Since these are new, some distros
   may not have header files which contain them. */
#ifndef HIDIOCSFEATURE
#define HIDIOCSFEATURE(len)    _IOC(_IOC_WRITE|_IOC_READ, 'H', 0x06, len)
#endif
#ifndef HIDIOCGFEATURE
#define HIDIOCGFEATURE(len)    _IOC(_IOC_WRITE|_IOC_READ, 'H', 0x07, len)
#endif

struct hid_device_ {
	int device_handle;
	int blocking;
	int uses_numbered_reports;
};


static __u32 kernel_version = 0;

hid_device *new_hid_device()
{
	hid_device *dev = calloc(1, sizeof(hid_device));
	dev->device_handle = -1;
	dev->blocking = 1;
	dev->uses_numbered_reports = 0;

	return dev;
}



/* Get an attribute value from a udev_device and return it as a whar_t
   string. The returned string must be freed with free() when done.*/
static wchar_t *copy_udev_string(struct udev_device *dev, const char *udev_name)
{
	const char *str;
	wchar_t *ret = NULL;
	str = udev_device_get_sysattr_value(dev, udev_name);
	if (str) {
		/* Convert the string from UTF-8 to wchar_t */
		size_t wlen = mbstowcs(NULL, str, 0);
		ret = calloc(wlen+1, sizeof(wchar_t));
		mbstowcs(ret, str, wlen+1);
		ret[wlen] = 0x0000;
	}
	
	return ret;
}

/* uses_numbered_reports() returns 1 if report_descriptor describes a device
   which contains numbered reports. */ 
static int uses_numbered_reports(__u8 *report_descriptor, __u32 size) {
	int i = 0;
	int size_code;
	int data_len, key_size;
	
	while (i < size) {
		int key = report_descriptor[i];

		/* Check for the Report ID key */
		if (key == 0x85/*Report ID*/) {
			/* This device has a Report ID, which means it uses
			   numbered reports. */
			return 1;
		}
		
		//printf("key: %02hhx\n", key);
		
		if ((key & 0xf0) == 0xf0) {
			/* This is a Long Item. The next byte contains the
			   length of the data section (value) for this key.
			   See the HID specification, version 1.11, section
			   6.2.2.3, titled "Long Items." */
			if (i+1 < size)
				data_len = report_descriptor[i+1];
			else
				data_len = 0; /* malformed report */
			key_size = 3;
		}
		else {
			/* This is a Short Item. The bottom two bits of the
			   key contain the size code for the data section
			   (value) for this key.  Refer to the HID
			   specification, version 1.11, section 6.2.2.2,
			   titled "Short Items." */
			size_code = key & 0x3;
			switch (size_code) {
			case 0:
			case 1:
			case 2:
				data_len = size_code;
				break;
			case 3:
				data_len = 4;
				break;
			default:
				/* Can't ever happen since size_code is & 0x3 */
				data_len = 0;
				break;
			};
			key_size = 1;
		}
		
		/* Skip over this key and it's associated data */
		i += data_len + key_size;
	}
	
	/* Didn't find a Report ID key. Device doesn't use numbered reports. */
	return 0;
}

static int get_device_string(hid_device *dev, const char *key, wchar_t *string, size_t maxlen)
{
	struct udev *udev;
	struct udev_device *udev_dev, *parent;
	struct stat s;
	int ret = -1;
	
	setlocale(LC_ALL,"");

	/* Create the udev object */
	udev = udev_new();
	if (!udev) {
		printf("Can't create udev\n");
		return -1;
	}

	/* Get the dev_t (major/minor numbers) from the file handle. */
	fstat(dev->device_handle, &s);
	/* Open a udev device from the dev_t. 'c' means character device. */
	udev_dev = udev_device_new_from_devnum(udev, 'c', s.st_rdev);
	if (udev_dev) {
		const char *str;
		/* Find the parent USB Device */
		parent = udev_device_get_parent_with_subsystem_devtype(
		       udev_dev,
		       "usb",
		       "usb_device");
		if (parent) {
			str = udev_device_get_sysattr_value(parent, key);
			if (str) {
				/* Convert the string from UTF-8 to wchar_t */
				ret = mbstowcs(string, str, maxlen);
				goto end;
			}
		}
	}

end:
	udev_device_unref(udev_dev);
	// parent doesn't need to be (and can't be) unref'd.
	// I'm not sure why, but it'll throw double-free() errors.
	udev_unref(udev);

	return ret;
}

int HID_API_EXPORT hid_init(void)
{
	/* Nothing to do for this in the Linux/hidraw implementation. */
	return 0;
}

int HID_API_EXPORT hid_exit(void)
{
	/* Nothing to do for this in the Linux/hidraw implementation. */
	return 0;
}

struct hid_device_info  HID_API_EXPORT *hid_enumerate(unsigned short vendor_id, unsigned short product_id)
{
	struct udev *udev;
	struct udev_enumerate *enumerate;
	struct udev_list_entry *devices, *dev_list_entry;
	
	struct hid_device_info *root = NULL; // return object
	struct hid_device_info *cur_dev = NULL;
	
	setlocale(LC_ALL,"");

	/* Create the udev object */
	udev = udev_new();
	if (!udev) {
		printf("Can't create udev\n");
		return NULL;
	}

	/* Create a list of the devices in the 'hidraw' subsystem. */
	enumerate = udev_enumerate_new(udev);
	udev_enumerate_add_match_subsystem(enumerate, "hidraw");
	udev_enumerate_scan_devices(enumerate);
	devices = udev_enumerate_get_list_entry(enumerate);
	/* For each item, see if it matches the vid/pid, and if so
	   create a udev_device record for it */
	udev_list_entry_foreach(dev_list_entry, devices) {
		const char *sysfs_path;
		const char *dev_path;
		const char *str;
		struct udev_device *hid_dev; // The device's HID udev node.
		struct udev_device *dev; // The actual hardware device.
		struct udev_device *intf_dev; // The device's interface (in the USB sense).
		unsigned short dev_vid;
		unsigned short dev_pid;
		
		/* Get the filename of the /sys entry for the device
		   and create a udev_device object (dev) representing it */
		sysfs_path = udev_list_entry_get_name(dev_list_entry);
		hid_dev = udev_device_new_from_syspath(udev, sysfs_path);
		dev_path = udev_device_get_devnode(hid_dev);
		
		/* The device pointed to by hid_dev contains information about
		   the hidraw device. In order to get information about the
		   USB device, get the parent device with the
		   subsystem/devtype pair of "usb"/"usb_device". This will
		   be several levels up the tree, but the function will find
		   it.*/
		dev = udev_device_get_parent_with_subsystem_devtype(
		       hid_dev,
		       "usb",
		       "usb_device");
		if (!dev) {
			/* Unable to find parent usb device. */
			goto next;
		}

		/* Get the VID/PID of the device */
		str = udev_device_get_sysattr_value(dev,"idVendor");
		dev_vid = (str)? strtol(str, NULL, 16): 0x0;
		str = udev_device_get_sysattr_value(dev, "idProduct");
		dev_pid = (str)? strtol(str, NULL, 16): 0x0;

		/* Check the VID/PID against the arguments */
		if ((vendor_id == 0x0 && product_id == 0x0) ||
		    (vendor_id == dev_vid && product_id == dev_pid)) {
			struct hid_device_info *tmp;
			size_t len;

		    	/* VID/PID match. Create the record. */
			tmp = malloc(sizeof(struct hid_device_info));
			if (cur_dev) {
				cur_dev->next = tmp;
			}
			else {
				root = tmp;
			}
			cur_dev = tmp;
			
			/* Fill out the record */
			cur_dev->next = NULL;
			str = dev_path;
			if (str) {
				len = strlen(str);
				cur_dev->path = calloc(len+1, sizeof(char));
				strncpy(cur_dev->path, str, len+1);
				cur_dev->path[len] = '\0';
			}
			else
				cur_dev->path = NULL;
			
			/* Serial Number */
			cur_dev->serial_number
				= copy_udev_string(dev, "serial");

			/* Manufacturer and Product strings */
			cur_dev->manufacturer_string
				= copy_udev_string(dev, "manufacturer");
			cur_dev->product_string
				= copy_udev_string(dev, "product");
			
			/* VID/PID */
			cur_dev->vendor_id = dev_vid;
			cur_dev->product_id = dev_pid;

			/* Release Number */
			str = udev_device_get_sysattr_value(dev, "bcdDevice");
			cur_dev->release_number = (str)? strtol(str, NULL, 16): 0x0;
			
			/* Interface Number */
			cur_dev->interface_number = -1;
			/* Get a handle to the interface's udev node. */
			intf_dev = udev_device_get_parent_with_subsystem_devtype(
				   hid_dev,
				   "usb",
				   "usb_interface");
			if (intf_dev) {
				str = udev_device_get_sysattr_value(intf_dev, "bInterfaceNumber");
				cur_dev->interface_number = (str)? strtol(str, NULL, 16): -1;
			}
		}
		else
			goto next;

	next:
		udev_device_unref(hid_dev);
		/* dev and intf_dev don't need to be (and can't be)
		   unref()d.  It will cause a double-free() error.  I'm not
		   sure why.  */
	}
	/* Free the enumerator and udev objects. */
	udev_enumerate_unref(enumerate);
	udev_unref(udev);
	
	return root;
}

void  HID_API_EXPORT hid_free_enumeration(struct hid_device_info *devs)
{
	struct hid_device_info *d = devs;
	while (d) {
		struct hid_device_info *next = d->next;
		free(d->path);
		free(d->serial_number);
		free(d->manufacturer_string);
		free(d->product_string);
		free(d);
		d = next;
	}
}

hid_device * hid_open(unsigned short vendor_id, unsigned short product_id, wchar_t *serial_number)
{
	struct hid_device_info *devs, *cur_dev;
	const char *path_to_open = NULL;
	hid_device *handle = NULL;
	
	devs = hid_enumerate(vendor_id, product_id);
	cur_dev = devs;
	while (cur_dev) {
		if (cur_dev->vendor_id == vendor_id &&
		    cur_dev->product_id == product_id) {
			if (serial_number) {
				if (wcscmp(serial_number, cur_dev->serial_number) == 0) {
					path_to_open = cur_dev->path;
					break;
				}
			}
			else {
				path_to_open = cur_dev->path;
				break;
			}
		}
		cur_dev = cur_dev->next;
	}

	if (path_to_open) {
		/* Open the device */
		handle = hid_open_path(path_to_open);
	}

	hid_free_enumeration(devs);
	
	return handle;
}

hid_device * HID_API_EXPORT hid_open_path(const char *path)
{
	hid_device *dev = NULL;

	dev = new_hid_device();

	if (kernel_version == 0) {
		struct utsname name;
		int major, minor, release;
		int ret;
		uname(&name);
		ret = sscanf(name.release, "%d.%d.%d", &major, &minor, &release);
		if (ret == 3) {
			kernel_version = major << 16 | minor << 8 | release;
			//printf("Kernel Version: %d\n", kernel_version);
		}
		else {
			printf("Couldn't sscanf() version string %s\n", name.release);
		}
	}

	// OPEN HERE //
	dev->device_handle = open(path, O_RDWR);

	// If we have a good handle, return it.
	if (dev->device_handle > 0) {

		/* Get the report descriptor */
		int res, desc_size = 0;
		struct hidraw_report_descriptor rpt_desc;

		memset(&rpt_desc, 0x0, sizeof(rpt_desc));

		/* Get Report Descriptor Size */
		res = ioctl(dev->device_handle, HIDIOCGRDESCSIZE, &desc_size);
		if (res < 0)
			perror("HIDIOCGRDESCSIZE");


		/* Get Report Descriptor */
		rpt_desc.size = desc_size;
		res = ioctl(dev->device_handle, HIDIOCGRDESC, &rpt_desc);
		if (res < 0) {
			perror("HIDIOCGRDESC");
		} else {
			/* Determine if this device uses numbered reports. */
			dev->uses_numbered_reports =
				uses_numbered_reports(rpt_desc.value,
				                      rpt_desc.size);
		}
		
		return dev;
	}
	else {
		// Unable to open any devices.
		free(dev);
		return NULL;
	}
}


int HID_API_EXPORT hid_write(hid_device *dev, const unsigned char *data, size_t length)
{
	int bytes_written;

	bytes_written = write(dev->device_handle, data, length);

	return bytes_written;
}


int HID_API_EXPORT hid_read_timeout(hid_device *dev, unsigned char *data, size_t length, int milliseconds)
{
	int bytes_read;

	if (milliseconds != 0) {
		/* milliseconds is -1 or > 0. In both cases, we want to
		   call poll() and wait for data to arrive. -1 means
		   INFINITE. */
		int ret;
		struct pollfd fds;

		fds.fd = dev->device_handle;
		fds.events = POLLIN;
		fds.revents = 0;
		ret = poll(&fds, 1, milliseconds);
		if (ret == -1 || ret == 0)
			/* Error or timeout */
			return ret;
	}

	bytes_read = read(dev->device_handle, data, length);
	if (bytes_read < 0 && errno == EAGAIN)
		bytes_read = 0;
	
	if (bytes_read >= 0 &&
	    kernel_version < KERNEL_VERSION(2,6,34) &&
	    dev->uses_numbered_reports) {
		/* Work around a kernel bug. Chop off the first byte. */
		memmove(data, data+1, bytes_read);
		bytes_read--;
	}

	return bytes_read;
}

int HID_API_EXPORT hid_read(hid_device *dev, unsigned char *data, size_t length)
{
	return hid_read_timeout(dev, data, length, (dev->blocking)? -1: 0);
}

int HID_API_EXPORT hid_set_nonblocking(hid_device *dev, int nonblock)
{
	int flags, res;

	flags = fcntl(dev->device_handle, F_GETFL, 0);
	if (flags >= 0) {
		if (nonblock)
			res = fcntl(dev->device_handle, F_SETFL, flags | O_NONBLOCK);
		else
			res = fcntl(dev->device_handle, F_SETFL, flags & ~O_NONBLOCK);
	}
	else
		return -1;

	if (res < 0) {
		return -1;
	}
	else {
		dev->blocking = !nonblock;
		return 0; /* Success */
	}
}


int HID_API_EXPORT hid_send_feature_report(hid_device *dev, const unsigned char *data, size_t length)
{
	int res;

	res = ioctl(dev->device_handle, HIDIOCSFEATURE(length), data);
	if (res < 0)
		perror("ioctl (SFEATURE)");

	return res;
}

int HID_API_EXPORT hid_get_feature_report(hid_device *dev, unsigned char *data, size_t length)
{
	int res;

	res = ioctl(dev->device_handle, HIDIOCGFEATURE(length), data);
	if (res < 0)
		perror("ioctl (GFEATURE)");


	return res;
}


void HID_API_EXPORT hid_close(hid_device *dev)
{
	if (!dev)
		return;
	close(dev->device_handle);
	free(dev);
}


int HID_API_EXPORT_CALL hid_get_manufacturer_string(hid_device *dev, wchar_t *string, size_t maxlen)
{
	return get_device_string(dev, "manufacturer", string, maxlen);
}

int HID_API_EXPORT_CALL hid_get_product_string(hid_device *dev, wchar_t *string, size_t maxlen)
{
	return get_device_string(dev, "product", string, maxlen);
}

int HID_API_EXPORT_CALL hid_get_serial_number_string(hid_device *dev, wchar_t *string, size_t maxlen)
{
	return get_device_string(dev, "serial", string, maxlen);
}

int HID_API_EXPORT_CALL hid_get_indexed_string(hid_device *dev, int string_index, wchar_t *string, size_t maxlen)
{
	return -1;
}


HID_API_EXPORT const wchar_t * HID_API_CALL  hid_error(hid_device *dev)
{
	return NULL;
}
