////////////////////////////////////////////////////////////////////////////////
// eval.c
// D. Everhart
// 27 JUN 2016
////////////////////////////////////////////////////////////////////////////////
//
// The MIT License (MIT)
// 
// Copyright (c) 2016 Daniel Everhart
// 
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the 
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject
// to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
////////////////////////////////////////////////////////////////////////////////
#include <stdio.h>
#include <string.h>

#define __DEBUG__ 1

// Actions
#define         ERR           0
#define         NEW         101
#define        PUSH         102
#define        FLSH         103
                          
// States                 
#define        UNKN           0
#define        WHIT         101
#define        IDEN         200
#define        INTG         301
#define        FLOT         302
#define         SCI         303
#define        SCIS         304
#define        NUMB         300
#define         POW         401
#define         MUL         402
#define         DIV         403
#define        FDIV         404
#define        MODU         405
#define         ADD         406
#define         SUB         407
#define        OPER         400
#define         GRP         500
#define        OGRP         510
#define        CGRP         520
#define        OPAR         511
#define        CPAR         521
#define        DELM         600
#define        COMA         601
#define        FUNC         700
                          
#define      BUFFSZ          64
#define        NRWS         150
#define        MTOK         128

struct _TOKEN_DESCRIPTOR
{
  size_t state;
  union
  {
    size_t start;
    double dval;
  };
  size_t end;
};
typedef struct _TOKEN_DESCRIPTOR TOKDES;

size_t table[NRWS][5] = {
//                STATE  START    END ACTION NXTSTATE                //
     {             UNKN,    10,    10,   NEW,   WHIT        },       //    \n   
     {             UNKN,    32,    32,   NEW,   WHIT        },       //    SPC  
     {             UNKN,    46,    46,   NEW,   FLOT        },       //     .   
     {             UNKN,    48,    57,   NEW,   INTG        },       //   0 - 9 
     {             UNKN,    65,    90,   NEW,   IDEN        },       //   A - Z 
     {             UNKN,    95,    95,   NEW,   IDEN        },       //     _   
     {             UNKN,    97,   122,   NEW,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             WHIT,    10,    10,  PUSH,   WHIT        },       //    \n   
     {             WHIT,    32,    32,  PUSH,   WHIT        },       //    SPC  
     {             WHIT,    37,    37,  FLSH,   MODU        },       //     %   
     {             WHIT,    40,    40,  FLSH,   OPAR        },       //     (   
     {             WHIT,    41,    41,  FLSH,   CPAR        },       //     )   
     {             WHIT,    42,    42,  FLSH,    MUL        },       //     *   
     {             WHIT,    43,    43,  FLSH,    ADD        },       //     +   
     {             WHIT,    45,    45,  FLSH,    SUB        },       //     -   
     {             WHIT,    46,    46,  FLSH,   FLOT        },       //     .   
     {             WHIT,    47,    47,  FLSH,    DIV        },       //     /   
     {             WHIT,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {             WHIT,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {             WHIT,    92,    92,  FLSH,   FDIV        },       //     \   
     {             WHIT,    94,    94,  FLSH,    POW        },       //     ^   
     {             WHIT,    95,    95,  FLSH,   IDEN        },       //     _   
     {             WHIT,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             IDEN,    10,    10,  FLSH,   WHIT        },       //    \n   
     {             IDEN,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             IDEN,    40,    40,  FLSH,   OPAR        },       //     (   
     {             IDEN,    41,    41,  FLSH,   CPAR        },       //     )   
     {             IDEN,    42,    42,  FLSH,    MUL        },       //     *   
     {             IDEN,    43,    43,  FLSH,    ADD        },       //     +   
     {             IDEN,    45,    45,  FLSH,    SUB        },       //     -   
     {             IDEN,    47,    47,  FLSH,    DIV        },       //     /   
     {             IDEN,    48,    57,  PUSH,   IDEN        },       //   0 - 9 
     {             IDEN,    65,    90,  PUSH,   IDEN        },       //   A - Z 
     {             IDEN,    94,    94,  FLSH,    POW        },       //     ^   
     {             IDEN,    95,    95,  PUSH,   IDEN        },       //     _   
     {             IDEN,    97,   122,  PUSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             INTG,    10,    10,  FLSH,   WHIT        },       //    \n   
     {             INTG,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             INTG,    37,    37,  FLSH,   MODU        },       //     %   
     {             INTG,    41,    41,  FLSH,   CPAR        },       //     )   
     {             INTG,    42,    42,  FLSH,    MUL        },       //     *   
     {             INTG,    43,    43,  FLSH,    ADD        },       //     +   
     {             INTG,    44,    44,  FLSH,   COMA        },       //     +   
     {             INTG,    45,    45,  FLSH,    SUB        },       //     -   
     {             INTG,    47,    47,  FLSH,    DIV        },       //     /   
     {             INTG,    46,    46,  PUSH,   FLOT        },       //     .   
     {             INTG,    48,    57,  PUSH,   INTG        },       //   0 - 9 
     {             INTG,    92,    92,  FLSH,   FDIV        },       //     \   
     {             INTG,    94,    94,  FLSH,    POW        },       //     ^   
     {             INTG,   101,   101,  PUSH,    SCI        },       //     e   
// -----------------------------------------------------------------------------
     {             FLOT,    10,    10,  FLSH,   WHIT        },       //    \n   
     {             FLOT,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             FLOT,    37,    37,  FLSH,   MODU        },       //     %   
     {             FLOT,    41,    41,  FLSH,   CPAR        },       //     )   
     {             FLOT,    42,    42,  FLSH,    MUL        },       //     *   
     {             FLOT,    43,    43,  FLSH,    ADD        },       //     +   
     {             FLOT,    45,    45,  FLSH,    SUB        },       //     -   
     {             FLOT,    47,    47,  FLSH,    DIV        },       //     /   
     {             FLOT,    48,    57,  PUSH,   FLOT        },       //   0 - 9 
     {             FLOT,    92,    92,  FLSH,   FDIV        },       //     \   
     {             FLOT,    94,    94,  FLSH,    POW        },       //     ^   
     {             FLOT,   101,   101,  PUSH,    SCI        },       //     e   
// -----------------------------------------------------------------------------
     {              SCI,    43,    43,  PUSH,   SCIS        },       //     +   
     {              SCI,    45,    45,  PUSH,   SCIS        },       //     -   
     {              SCI,    48,    57,  PUSH,   SCIS        },       //   0 - 9 
// -----------------------------------------------------------------------------
     {             SCIS,    10,    10,  FLSH,   WHIT        },       //    \n   
     {             SCIS,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             SCIS,    37,    37,  FLSH,   MODU        },       //     %   
     {             SCIS,    41,    41,  FLSH,   CPAR        },       //     )   
     {             SCIS,    42,    42,  FLSH,    MUL        },       //     *   
     {             SCIS,    43,    43,  FLSH,    ADD        },       //     +   
     {             SCIS,    45,    45,  FLSH,    SUB        },       //     -   
     {             SCIS,    47,    47,  FLSH,    DIV        },       //     /   
     {             SCIS,    48,    57,  PUSH,   SCIS        },       //   0 - 9 
     {             SCIS,    92,    92,  FLSH,   FDIV        },       //     \   
     {             SCIS,    94,    94,  FLSH,    POW        },       //     ^   
// -----------------------------------------------------------------------------
     {              POW,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {              POW,    40,    40,  FLSH,   OPAR        },       //     (   
     {              POW,    46,    46,  FLSH,   FLOT        },       //     .   
     {              POW,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {              POW,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {              POW,    95,    95,  FLSH,   IDEN        },       //     _   
     {              POW,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {              MUL,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {              MUL,    40,    40,  FLSH,   OPAR        },       //     (   
     {              MUL,    42,    42,  PUSH,    POW        },       //     *   
     {              MUL,    46,    46,  FLSH,   FLOT        },       //     .   
     {              MUL,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {              MUL,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {              MUL,    95,    95,  FLSH,   IDEN        },       //     _   
     {              MUL,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {              DIV,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {              DIV,    40,    40,  FLSH,   OPAR        },       //     (   
     {              DIV,    46,    46,  FLSH,   FLOT        },       //     .   
     {              DIV,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {              DIV,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {              DIV,    95,    95,  FLSH,   IDEN        },       //     _   
     {              DIV,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             FDIV,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             FDIV,    40,    40,  FLSH,   OPAR        },       //     (   
     {             FDIV,    46,    46,  FLSH,   FLOT        },       //     .   
     {             FDIV,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {             FDIV,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {             FDIV,    95,    95,  FLSH,   IDEN        },       //     _   
     {             FDIV,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             MODU,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             MODU,    40,    40,  FLSH,   OPAR        },       //     (   
     {             MODU,    46,    46,  FLSH,   FLOT        },       //     .   
     {             MODU,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {             MODU,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {             MODU,    95,    95,  FLSH,   IDEN        },       //     _   
     {             MODU,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {              ADD,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {              ADD,    40,    40,  FLSH,   OPAR        },       //     (   
     {              ADD,    46,    46,  FLSH,   FLOT        },       //     .   
     {              ADD,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {              ADD,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {              ADD,    95,    95,  FLSH,   IDEN        },       //     _   
     {              ADD,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {              SUB,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {              SUB,    40,    40,  FLSH,   OPAR        },       //     (   
     {              SUB,    46,    46,  FLSH,   FLOT        },       //     .   
     {              SUB,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {              SUB,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {              SUB,    95,    95,  FLSH,   IDEN        },       //     _   
     {              SUB,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             OPAR,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             OPAR,    40,    40,  FLSH,   OPAR        },       //     (   
     {             OPAR,    41,    41,  FLSH,   CPAR        },       //     )   
     {             OPAR,    46,    46,  FLSH,   FLOT        },       //     .   
     {             OPAR,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {             OPAR,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {             OPAR,    95,    95,  FLSH,   IDEN        },       //     _   
     {             OPAR,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {             CPAR,    10,    10,  FLSH,   WHIT        },       //    \n   
     {             CPAR,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             CPAR,    41,    41,  FLSH,   CPAR        },       //     )   
     {             CPAR,    42,    42,  FLSH,    MUL        },       //     *   
     {             CPAR,    43,    43,  FLSH,    ADD        },       //     +   
     {             CPAR,    45,    45,  FLSH,    SUB        },       //     -   
     {             CPAR,    47,    47,  FLSH,    DIV        },       //     /   
     {             CPAR,    94,    94,  FLSH,    POW        },       //     ^   
// -----------------------------------------------------------------------------
     {             COMA,    32,    32,  FLSH,   WHIT        },       //    SPC  
     {             COMA,    40,    40,  FLSH,   OPAR        },       //     (   
     {             COMA,    46,    46,  FLSH,   FLOT        },       //     .   
     {             COMA,    48,    57,  FLSH,   INTG        },       //   0 - 9 
     {             COMA,    65,    90,  FLSH,   IDEN        },       //   A - Z 
     {             COMA,    95,    95,  FLSH,   IDEN        },       //     _   
     {             COMA,    97,   122,  FLSH,   IDEN        },       //   a - z 
// -----------------------------------------------------------------------------
     {            99999, 99999, 99999,   ERR,   UNKN        },
};

//size_t toklst[MTOK][3];
TOKDES toklst[MTOK];
size_t ntok;

int scan(const char *, size_t);
int parse(const char *, size_t);
int printToken(const TOKDES *, const char *, size_t);

int main(int argc, char *argv[])
{
  //                            1           2         3         4         5
  //                  01234567890 12345 67890123456789012345678901234567890
  //char raw[BUFFSZ] = " CaLL _f3D \"1,7\"  4.e+3/-sin((42**-3)-pi);)";
  char raw[BUFFSZ];
  char *str;
  int res;

  while (!feof(stdin))
  {
    str = fgets(raw, BUFFSZ, stdin);
    if (str)
    {
      res = scan(raw, BUFFSZ);
#ifdef __DEBUG__
      printf("scan result: %d\n", res);
#endif
      if (res > 0)
      {
        res = parse(raw, BUFFSZ);
#ifdef __DEBUG__
        printf("parse result: %d\n", res);
#endif
      }
    }
  }
  return 0;
}

int scan(const char *str, size_t maxlen)
{
  size_t i,j,length,state,action,nextstate,c,start,end;

  length = strnlen(str,maxlen);
  state = UNKN;
  action = ERR;
  
  ntok = 0;
  memset(toklst,0,MTOK*sizeof(TOKDES));

#ifdef __DEBUG__
  printf("input string:  %s\n",str);
  printf("----  sizeof(c):  -->%d<--\n", sizeof(c));
#endif 

  for (i=0; i<length; i++)
  {
    c = (size_t)str[i];
    for (j=0; j<NRWS; j++)
    {
      if (table[j][0] == state)
      {
        if (   (table[j][1] <= c) &&
               (table[j][2] >= c)     )
        {
          action    = table[j][3];
          nextstate = table[j][4];
          break;
        }
      }
      else if (table[j][0] > state)
      {
        printf("0*** ERROR AT CHAR   %d, %d\n",i,c);
        return -i;
      }
    }

#ifdef __DEBUG__
    printf("%c, %d, %d, %d\n",str[i],state,action,nextstate);
#endif

    switch (action)
    {
      case ERR:
      {
        printf("Error:  %d\n",action);
        return -i;
        break;
      }
      case NEW:
      {
        start = i;
        end = start;
        break;
      }
      case PUSH:
      {
        end++;
        break;
      }
      case FLSH:
      {
        if (state != WHIT)
        {
          toklst[ntok].state = state;
          toklst[ntok].start = start;
          toklst[ntok].end   = end;
#ifdef __DEBUG__
          printToken(&toklst[ntok], str, BUFFSZ);
#endif
          ntok++;
        }
        start = i;
        end = start;
        break;
      }
      default:
      {
        printf("Unknown Action:  %d\n",action);
        return -i;
      }
    }
    state = nextstate;
  }

//                       Not sure if this part of the check
//                       is actually required....
//                        |           |
//                        v           v
  if ((state != WHIT) && (end >= start))
  {
#ifdef __DEBUG__
    printf("TOKEN: -->");
    for (j=start; j<=end; j++)
    {
      printf("%c",str[j]);
    }
    printf("<--\n");
#endif

    toklst[ntok].state = state;
    toklst[ntok].start = start;
    toklst[ntok].end   = end;
    ntok++;
  }
  return i;
}

int parse(const char *str, size_t maxlen)
{
  int i,j;
  size_t ns,nq,start,length;
  TOKDES q[MTOK],s[MTOK];
  double dval[2];
  char buff[BUFFSZ];

  ns = 0;
  nq = 0;

  // Begin building POSTFIX queue from INFIX.
  for (i=0; i<ntok; i++)
  {
    switch (100*(toklst[i].state/100))
    {
      case IDEN:
      case NUMB:
      {
        memcpy(&(q[nq]),&(toklst[i]),sizeof(TOKDES));
        nq++;
        break;
      }
      case OPER:
      {
        if (ns == 0)
        {
          // Push operator onto stack.
          memcpy(&(s[ns]),&(toklst[i]),sizeof(TOKDES));
          ns++;
        }
        else
        {
          if (s[ns-1].state <= toklst[i].state)
          {
            ns--;
            // Pop operator from stack into queue.
            memcpy(&(q[nq]),&(s[ns]),sizeof(TOKDES));
            nq++;
            // Push operator onto stack.
            memcpy(&(s[ns]),&(toklst[i]),sizeof(TOKDES));
            ns++;
          }
          else
          {
            memcpy(&(s[ns]),&(toklst[i]),sizeof(TOKDES));
            ns++;
          }
        }
        break;
      }
      default:
      {
        printf("Unknown State:  %d\n",toklst[i].state);
        return -i;
      }
    }
  }
    
  while (ns > 0)
  {
    ns--;
    memcpy(&(q[nq]),&(s[ns]),sizeof(TOKDES));
    nq++;
  }

#ifdef __DEBUG__
  printf("------------ POSTFIX ----------------\n");
  for (i=0; i<nq; i++) printToken(&q[i], str, BUFFSZ);
#endif

  // Evaluate POSTFIX queue.
  for (i=0; i<nq; i++)
  {
    switch (100*(q[i].state/100))
    {
      case NUMB:
      {
        memset(buff,0,BUFFSZ);
        start = q[i].start;
        length = q[i].end - start + 1;
        memcpy(buff, &(str[start]), length*sizeof(char));
        sscanf(buff,"%lf",&(s[ns].dval));
        s[ns].state = q[i].state;
        s[ns].end = 0;
        ns++;
        break;
      }
      case OPER:
      {
        ns--;
        dval[1] = s[ns].dval;
        ns--;
        dval[0] = s[ns].dval;
        switch (q[i].state)
        {
          case MUL:
          {
            dval[0] = dval[0] * dval[1];
            break;
          }
          case ADD:
          {
            dval[0] = dval[0] + dval[1];
            break;
          }
          default:
          {
            printf("Error unknow operator:  %d\n",q[i].state);
            break;
          }
        }
        s[ns].dval = dval[0];
        ns++;
        break;
      }
      default:
      {
        printf("Error token:  ");
        for (j=q[i].start; j<=q[i].end; j++) printf("%c",str[j]);
        printf("\n");
        return -i;
      }
    }
  }

  if (ns == 1)
  {
    ns--;
    dval[0] = s[ns].dval;
    printf("Value %lf\n", dval[0]);
  }
  else
  {
    printf("Error Evaluating Stack.  ns = %d\n",ns);
    return -1;
  }

  return i;
}

int printToken(const TOKDES *tokdes, const char *str, size_t maxlen)
{
  size_t j,len;
  if (!tokdes) return -1;
  if (!str) return -1;
  printf("TOKEN: -->");
  len = 0;
  for (j=tokdes->start; j<=tokdes->end; j++)
  {
    if (j < maxlen-1)
    {
      printf("%c",str[j]);
      len++;
    }
    else
    {
      break;
    }
  }
  printf("<--\n");
  return len;
}
