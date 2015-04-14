#ifndef __C_PATCH_C__
#define __C_PATCH_C__

#ifndef free
#define free
#endif

#ifdef gcc_obstack_init
#undef gcc_obstack_init
#endif

#ifndef OBSTACK_CHUNK_SIZE
#define OBSTACK_CHUNK_SIZE 4096
#endif

#ifndef OBSTACK_CHUNK_ALLOC
#define OBSTACK_CHUNK_ALLOC xmalloc
#endif

#ifndef OBSTACK_CHUNK_FREE
#define OBSTACK_CHUNK_FREE free
#endif

#ifdef gcc_obstack_init
#undef gcc_obstack_init
#endif
static incline
void
gcc_obstack_init (obstack)
{
  _obstack_begin (obstack, OBSTACK_CHUNK_SIZE, 0,
		  (void *(*) PARAMS ((long ))) OBSTACK_CHUNK_ALLOC,
		  NULL /*(void (*) PARAMS ((void *))) OBSTACK_CHUNK_FREE */);
}

#ifdef obstack_init
#undef obstack_init
#endif
#define obstack_init(h) \
_obstack_begin ((h), 0, 0, (void *(*) (long)) obstack_chunk_alloc, NULL/*(void (*) (void *)) obstack_chunk_free*/)

#endif
