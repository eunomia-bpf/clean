/* Catch Lean code-generator depth failures and preserve local-list order. */
#define CELL(P) volatile unsigned x##P = value;
#define TEN(P) CELL(P##0) CELL(P##1) CELL(P##2) CELL(P##3) CELL(P##4) \
               CELL(P##5) CELL(P##6) CELL(P##7) CELL(P##8) CELL(P##9)
#define HUNDRED(P) TEN(P##0) TEN(P##1) TEN(P##2) TEN(P##3) TEN(P##4) \
                   TEN(P##5) TEN(P##6) TEN(P##7) TEN(P##8) TEN(P##9)
unsigned many_variables(unsigned value)
{
    HUNDRED(a) HUNDRED(b) HUNDRED(c) HUNDRED(d)
    HUNDRED(e) HUNDRED(f) HUNDRED(g) HUNDRED(h)
    HUNDRED(i) HUNDRED(j) HUNDRED(k) HUNDRED(l)
    HUNDRED(m) HUNDRED(n) HUNDRED(o) HUNDRED(p)
    return xa00 + xp99;
}
#define REPEAT10(S) S S S S S S S S S S
#define REPEAT100(S) REPEAT10(REPEAT10(S))
extern unsigned next(unsigned);
unsigned many_temporaries(unsigned value)
{
    unsigned x = value;
    REPEAT100(REPEAT10(x += next(value);))
    REPEAT100(REPEAT10(x += next(value);))
    return x;
}
typedef unsigned (*handler)(void *, void *, void *, void *, void *);
extern handler next_handler(unsigned);
unsigned many_function_temporaries(unsigned value)
{
    unsigned x = value;
    REPEAT100(REPEAT10(x += next_handler(value)(0, 0, 0, 0, 0);))
    return x;
}
