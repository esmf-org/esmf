#ifndef _ESMFTRC_PLATFORM_LINUX_FS_H
#define _ESMFTRC_PLATFORM_LINUX_FS_H

/*
 * Header for "platform" for writing trace events to file.
 *
 * Copyright (c) 2015 EfficiOS Inc. and Linux Foundation
 * Copyright (c) 2015 Philippe Proulx <pproulx@efficios.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdint.h>
#include <esmftrc.h>

#ifdef __cplusplus
extern "C" {
#endif

struct esmftrc_platform_linux_fs_ctx;

/**
 * Initializes the platform.
 *
 * @param buf_size			Packet size (bytes)
 * @param trace_dir			Trace directory
 * @param localPet                      pet number
 * @returns				Platform context
 */
struct esmftrc_platform_linux_fs_ctx *esmftrc_platform_linux_fs_init(
  unsigned int buf_size, const char *trace_dir, int localPet);
  

int esmftrc_filesys_init(unsigned int buf_size, const char *trace_dir, int localPet);

/**
 * Finalizes the platform.
 *
 * @param ctx	Platform context
 */
void esmftrc_platform_linux_fs_fini(struct esmftrc_platform_linux_fs_ctx *ctx);

/**
 * Returns the esmftrc stream-specific context of a given platform context.
 *
 * This context is what esmftrc tracing functions need.
 *
 * @param ctx	Platform context
 * @returns	esmftrc stream-specific context
 */
struct esmftrc_default_ctx *esmftrc_platform_linux_fs_get_esmftrc_ctx(
	struct esmftrc_platform_linux_fs_ctx *ctx);

#ifdef __cplusplus
}
#endif

#endif /* _ESMFTRC_PLATFORM_LINUX_FS_H */
