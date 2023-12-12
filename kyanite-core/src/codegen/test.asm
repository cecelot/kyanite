0 .main:  
1 pushq rbp 
2 movq rsp rbp
3 movq $2 T2
4 movq $3 T3
5 movq T2 rdi
6 movq T3 rsi
7 callq qux
8 movq %rax -8(%rbp)
9 movq $10 T4
10 movq -8(%rbp) T5
11 cmp T4 T5
12 jge L0
13 .L1:  
14 movq $1 T6
15 je L6
16 .L10:  
17 jmp L7
18 .qux:  
19 pushq rbp 
20 movq rsp rbp
21 movq rdi -8(%rbp)
22 movq rsi -16(%rbp)
23 movq -16(%rbp) T0
24 movq -8(%rbp) T7
25 imulq T0 T7
26 movq T7 T1
27 movq T1 rax
28 jmp qux.epilogue
29 .L3:  
30 movq $8 T8
31 movq $8 T9
32 addq T8 T9
33 .L5:  
34 jmp L8
35 .L8:  
36 movq $12 T10
37 movq $12 T11
38 addq T10 T11
39 jmp main.epilogue
40 .L4:  
41 movq $9 T12
42 movq $9 T13
43 addq T12 T13
44 jmp L5
45 .L0:  
46 movq $6 T14
47 movq $6 T15
48 addq T14 T15
49 movq $10 T16
50 movq -8(%rbp) T17
51 cmp T16 T17
52 jge L3
53 .L9:  
54 jmp L4
55 .L6:  
56 movq $7 T18
57 movq $7 T19
58 addq T18 T19
59 jmp L8
60 .L7:  
61 movq $15 T20
62 movq $15 T21
63 addq T20 T21
64 jmp L8
65 .qux.epilogue:  
66 popq rbp 
67 retq  
68 .main.epilogue:  
69 popq rbp 
70 retq  