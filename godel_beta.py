import math

def beta(seq, idx):
	seq.append(len(seq));
	d = math.factorial(max(seq));
	seq.pop(); #because of append in first line
	dlist = map(lambda i : 1 + (i+1)*d, range(0,len(seq)));
	print dlist;
	p = reduce(lambda x,y : x*y, dlist);
	print p
	for c in range(0,p-1):
		remlist = map(lambda x : c % x, dlist);
		if remlist == seq:
			return c % (dlist.pop(idx))
