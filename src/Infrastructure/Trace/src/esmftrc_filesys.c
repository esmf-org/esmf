/*
 * 
 * Writes trace events to the file system.
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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>
#include <esmftrc.h>
#include <time.h>

#include "esmftrc_filesys.h"

#ifdef __cplusplus
# define TO_VOID_PTR(_value)		static_cast<void *>(_value)
# define FROM_VOID_PTR(_type, _value)	static_cast<_type *>(_value)
#else
# define TO_VOID_PTR(_value)		((void *) (_value))
# define FROM_VOID_PTR(_type, _value)	((_type *) (_value))
#endif

struct esmftrc_platform_linux_fs_ctx {
	struct esmftrc_default_ctx ctx;
	FILE *fh;
	int localPet;
};

static uint32_t get_clock(void* data)
{
	struct timespec ts;

	clock_gettime(CLOCK_MONOTONIC, &ts);

	return ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}

static void write_packet(struct esmftrc_platform_linux_fs_ctx *ctx)
{
	size_t nmemb = fwrite(esmftrc_packet_buf(&ctx->ctx),
		esmftrc_packet_buf_size(&ctx->ctx), 1, ctx->fh);
	assert(nmemb == 1);
}

static int is_backend_full(void *data)
{
	struct esmftrc_platform_linux_fs_ctx *ctx =
		FROM_VOID_PTR(struct esmftrc_platform_linux_fs_ctx, data);

	return 0;
}

static void open_packet(void *data)
{
	struct esmftrc_platform_linux_fs_ctx *ctx =
		FROM_VOID_PTR(struct esmftrc_platform_linux_fs_ctx, data);

	esmftrc_default_open_packet(&ctx->ctx, 0);
}

static void close_packet(void *data)
{
	struct esmftrc_platform_linux_fs_ctx *ctx =
		FROM_VOID_PTR(struct esmftrc_platform_linux_fs_ctx, data);

	/* close packet now */
	esmftrc_default_close_packet(&ctx->ctx);

	/* write packet to file */
	write_packet(ctx);
}

struct esmftrc_platform_linux_fs_ctx *esmftrc_platform_linux_fs_init(
	unsigned int buf_size, const char *trace_dir, int localPet)
{
	char stream_path[256];
	uint8_t *buf;
	struct esmftrc_platform_linux_fs_ctx *ctx;
	struct esmftrc_platform_callbacks cbs;

	cbs.sys_clock_clock_get_value = get_clock;
	cbs.is_backend_full = is_backend_full;
	cbs.open_packet = open_packet;
	cbs.close_packet = close_packet;
	ctx = FROM_VOID_PTR(struct esmftrc_platform_linux_fs_ctx, malloc(sizeof(*ctx)));

	if (!ctx) {
		return NULL;
	}

	buf = FROM_VOID_PTR(uint8_t, malloc(buf_size));

	if (!buf) {
		free(ctx);
		return NULL;
	}

	memset(buf, 0, buf_size);

	sprintf(stream_path, "%s/stream%d", trace_dir, localPet);
	ctx->fh = fopen(stream_path, "wb");

	if (!ctx->fh) {
		free(ctx);
		free(buf);
		return NULL;
	}

	ctx->localPet = localPet;

	esmftrc_init(&ctx->ctx, buf, buf_size, cbs, ctx);
	open_packet(ctx);

	return ctx;
}

//assume just one context for now
struct esmftrc_platform_linux_fs_ctx *global_esmftrc_filesys_ctx;

int esmftrc_filesys_init(unsigned int buf_size, const char *trace_dir, int localPet) {
  global_esmftrc_filesys_ctx = esmftrc_platform_linux_fs_init(buf_size, trace_dir, localPet);
  if (global_esmftrc_filesys_ctx==NULL) {
    return 0;
  }
  else {
    return 1;
  }
}

void esmftrc_platform_linux_fs_fini(struct esmftrc_platform_linux_fs_ctx *ctx)
{
	if (esmftrc_packet_is_open(&ctx->ctx) &&
			!esmftrc_packet_is_empty(&ctx->ctx)) {
		close_packet(ctx);
	}

	fclose(ctx->fh);
	free(esmftrc_packet_buf(&ctx->ctx));
	free(ctx);
}

struct esmftrc_default_ctx *esmftrc_platform_linux_fs_get_esmftrc_ctx(
	struct esmftrc_platform_linux_fs_ctx *ctx)
{
	return &ctx->ctx;
}
