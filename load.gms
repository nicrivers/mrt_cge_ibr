$if defined ii_ $abort "The loadrm(r) macro requires a uniquely defined alias for i."
$if defined ss_ $abort "The loadrm(r) macro requires a uniquely defined alias for s."

alias (r,r_,rr), (j,j_),(i,ii_), (s,ss_), (f,f_);

$macro loadrm(r) \
	rm(r_) = no; \
	rm(r) = yes; \
	rnum(r) = yes$(rm(r) and (vom("c",r)+vom("sd",r)+vom("dd",r)= \
			  smax(rm,vom("c",rm)+vom("sd",rm)+vom("dd",rm)))); \
	rx(r_) = (not rm(r_)); \
	vem(ii_,r_) = sum(rx,vxmd(ii_,r_,rx))$rm(r_) + sum(rm,vxmd(ii_,r_,rm))$rx(r_); \
	vem(ii_,r_)$(not round(vem(ii_,r_),5)) = 0; \
	rtxs_row(ii_,r_) = (sum(rx,vxmd(ii_,r_,rx)*rtxs(ii_,r_,rx))/vem(ii_,r_))$(rm(r_) and vem(ii_,r_));\
	rowpfx = sum(rx, vom("c",rx)+vom("sd",rx)+vom("dd",rx)+ \
		 sum(ii_,vcm(ii_,rx))+sum((j_,ii_,ss_),vtwr(j_,ii_,ss_,rx))) + sum(rm,vb(rm));\
	pem0(ii_,r_) = (1-rtxs_row(ii_,r_))$rm(r_);
