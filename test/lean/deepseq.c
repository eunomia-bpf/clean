/* Regression: long straight-line Clight sequences must export as Lean terms
   that typecheck at the default recursion depth. Unsigned arithmetic keeps
   every operation defined; the argument prevents constant folding. */
#define TWICE(X) X X
#define FOUR(X) TWICE(TWICE(X))
#define SIXTEEN(X) FOUR(FOUR(X))
#define TWO_FIFTY_SIX(X) SIXTEEN(SIXTEEN(X))

unsigned long long deepseq(unsigned long long value)
{
  TWO_FIFTY_SIX(value ^= value << 1;)
  return value;
}
