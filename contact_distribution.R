conw1a=subset(conw1,part_age>18&part_age<30)

conw1partid=conw1a$part_id

test=duplicated(conw1partid)
connum=table(conw1partid[test])
as.data.frame(connum)

describe(connum)
