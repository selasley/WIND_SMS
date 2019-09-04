	OPTIONS /EXTEND_SOURCE
	character pattern*50,result*80
	integer reslen,status,n
100	type *,'Enter file search pattern (no quotes, wildcards allowed)'
	accept 1001,n,pattern
1001	format(q,a)
	call select_file(pattern(1:n),' ',result,reslen,status)
	type *,'status=',status
	if (reslen .gt. 0) type *,'result=',result(1:reslen)
	goto 100
	end
