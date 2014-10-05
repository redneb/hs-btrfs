/*
 * Unfortunately, btrfs/ioctl.h (from btrfs-progs) and linux/btrfs.h (from
 * linux-headers) have some differences. We put here some parts that are
 * missing * from btrfs/ioctl.h.
 */

#ifndef MISSING_H
#define MISSING_H
#include <linux/types.h>
#include <linux/ioctl.h>

#ifdef __cplusplus
extern "C" {
#endif

#define BTRFS_SAME_DATA_DIFFERS	1

struct btrfs_ioctl_same_extent_info {
	__s64 fd;		/* in - destination file */
	__u64 logical_offset;	/* in - start of extent in destination */
	__u64 bytes_deduped;	/* out - total # of bytes we were able
				 * to dedupe from this file */
	/* status of this dedupe operation:
	 * 0 if dedup succeeds
	 * < 0 for error
	 * == BTRFS_SAME_DATA_DIFFERS if data differs
	 */
	__s32 status;		/* out - see above description */
	__u32 reserved;
};

struct btrfs_ioctl_same_args {
	__u64 logical_offset;	/* in - start of extent in source */
	__u64 length;		/* in - length of extent */
	__u16 dest_count;	/* in - total elements in info array */
	__u16 reserved1;
	__u32 reserved2;
	struct btrfs_ioctl_same_extent_info info[0];
};

#define BTRFS_IOC_FILE_EXTENT_SAME _IOWR(BTRFS_IOCTL_MAGIC, 54, \
					 struct btrfs_ioctl_same_args)

#ifdef __cplusplus
}
#endif

#endif
