
# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DIQ.htm
dm.data %>% filter(cycle == "1999-2000") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DIQ_B.htm
dm.data %>% filter(cycle == "2001-2002") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DIQ_C.htm
dm.data %>% filter(cycle == "2003-2004") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.htm
dm.data %>% filter(cycle == "2005-2006") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.htm
dm.data %>% filter(cycle == "2007-2008") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.htm
dm.data %>% filter(cycle == "2009-2010") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.htm
dm.data %>% filter(cycle == "2011-2012") %>% select(dm.label) %>% summary

# check diabetes data summary  against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.htm
dm.data %>% filter(cycle == "2013-2014") %>% select(dm.label) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10AM.htm
gluc1.data %>% filter(cycle == "1999-2000") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10AM_B.htm
gluc1.data %>% filter(cycle == "2001-2002") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10AM_C.htm
gluc1.data %>% filter(cycle == "2003-2004") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GLU_D.htm
gluc1.data %>% filter(cycle == "2005-2006") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/GLU_E.htm
gluc1.data %>% filter(cycle == "2007-2008") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/GLU_F.htm
gluc1.data %>% filter(cycle == "2009-2010") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GLU_G.htm
gluc1.data %>% filter(cycle == "2011-2012") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/GLU_H.htm
gluc1.data %>% filter(cycle == "2013-2014") %>% select(gluc1.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10.htm
gluc2.data %>% filter(cycle == "1999-2000") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10_B.htm
gluc2.data %>% filter(cycle == "2001-2002") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10_C.htm
gluc2.data %>% filter(cycle == "2003-2004") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GHB_D.htm
gluc2.data %>% filter(cycle == "2005-2006") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/GHB_E.htm
gluc2.data %>% filter(cycle == "2007-2008") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/GHB_F.htm
gluc2.data %>% filter(cycle == "2009-2010") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/GHB_G.htm
gluc2.data %>% filter(cycle == "2011-2012") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/GHB_H.htm
gluc2.data %>% filter(cycle == "2013-2014") %>% select(gluc2.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/OGTT_D.htm
gluc3.data %>% filter(cycle == "2005-2006") %>% select(gluc3.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/OGTT_E.htm
gluc3.data %>% filter(cycle == "2007-2008") %>% select(gluc3.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/OGTT_F.htm
gluc3.data %>% filter(cycle == "2009-2010") %>% select(gluc3.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OGTT_G.htm
gluc3.data %>% filter(cycle == "2011-2012") %>% select(gluc3.value) %>% summary

# check glucose data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/OGTT_H.htm
gluc3.data %>% filter(cycle == "2013-2014") %>% select(gluc3.value) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/MCQ.htm
hist.data %>% filter(cycle == "1999-2000") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/MCQ_B.htm
hist.data %>% filter(cycle == "2001-2002") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/MCQ_C.htm
hist.data %>% filter(cycle == "2003-2004") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/MCQ_D.htm
hist.data %>% filter(cycle == "2005-2006") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/MCQ_E.htm
hist.data %>% filter(cycle == "2007-2008") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/MCQ_F.htm
hist.data %>% filter(cycle == "2009-2010") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.htm
hist.data %>% filter(cycle == "2011-2012") %>% select(hist.label) %>% summary

# check diabetes data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.htm
hist.data %>% filter(cycle == "2013-2014") %>% select(hist.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.htm
demo.data %>% filter(cycle == "1999-2000") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.htm
demo.data %>% filter(cycle == "2001-2002") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.htm
demo.data %>% filter(cycle == "2003-2004") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.htm
demo.data %>% filter(cycle == "2005-2006") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.htm
demo.data %>% filter(cycle == "2007-2008") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.htm
demo.data %>% filter(cycle == "2009-2010") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm
demo.data %>% filter(cycle == "2011-2012") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check demographic data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm
demo.data %>% filter(cycle == "2013-2014") %>% select(age.value, gender.label, race.label, educ.label, preg.label) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.htm
body.data %>% filter(cycle == "1999-2000") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.htm
body.data %>% filter(cycle == "2001-2002") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.htm
body.data %>% filter(cycle == "2003-2004") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.htm
body.data %>% filter(cycle == "2005-2006") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.htm
body.data %>% filter(cycle == "2007-2008") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.htm
body.data %>% filter(cycle == "2009-2010") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.htm
body.data %>% filter(cycle == "2011-2012") %>% select(bmi.value, waist.value) %>% summary

# check body measures data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.htm
body.data %>% filter(cycle == "2013-2014") %>% select(bmi.value, waist.value) %>% summary

# check sleep data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/SLQ_D.htm
sleep.data %>% filter(cycle == "2005-2006") %>% select(sleep.value) %>% summary

# check sleep data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/SLQ_E.htm
sleep.data %>% filter(cycle == "2007-2008") %>% select(sleep.value) %>% summary

# check sleep data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SLQ_F.htm
sleep.data %>% filter(cycle == "2009-2010") %>% select(sleep.value) %>% summary

# check sleep data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SLQ_G.htm
sleep.data %>% filter(cycle == "2011-2012") %>% select(sleep.value) %>% summary

# check sleep data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SLQ_H.htm
sleep.data %>% filter(cycle == "2013-2014") %>% select(sleep.value) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BPQ.htm
bp.data %>% filter(cycle == "1999-2000") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BPQ_B.htm
bp.data %>% filter(cycle == "2001-2002") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BPQ_C.htm
bp.data %>% filter(cycle == "2003-2004") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.htm
bp.data %>% filter(cycle == "2005-2006") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPQ_E.htm
bp.data %>% filter(cycle == "2007-2008") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPQ_F.htm
bp.data %>% filter(cycle == "2009-2010") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.htm
bp.data %>% filter(cycle == "2011-2012") %>% select(bp.label, chol.label) %>% summary

# check blood pressure data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.htm
bp.data %>% filter(cycle == "2013-2014") %>% select(bp.label, chol.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/SMQ.htm
smoke.data %>% filter(cycle == "1999-2000") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/SMQ_B.htm
smoke.data %>% filter(cycle == "2001-2002") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/SMQ_C.htm
smoke.data %>% filter(cycle == "2003-2004") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/SMQ_D.htm
smoke.data %>% filter(cycle == "2005-2006") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/SMQ_E.htm
smoke.data %>% filter(cycle == "2007-2008") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SMQ_F.htm
smoke.data %>% filter(cycle == "2009-2010") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.htm
smoke.data %>% filter(cycle == "2011-2012") %>% select(smoke.label) %>% summary

# check smoking data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.htm
smoke.data %>% filter(cycle == "2013-2014") %>% select(smoke.label) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/ALQ.htm
alc.data %>% filter(cycle == "1999-2000") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/ALQ_B.htm
alc.data %>% filter(cycle == "2001-2002") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/ALQ_C.htm
alc.data %>% filter(cycle == "2003-2004") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/ALQ_D.htm
alc.data %>% filter(cycle == "2005-2006") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/ALQ_E.htm
alc.data %>% filter(cycle == "2007-2008") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/ALQ_F.htm
alc.data %>% filter(cycle == "2009-2010") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALQ_G.htm
alc.data %>% filter(cycle == "2011-2012") %>% select(alc.value) %>% summary

# check alcohol data summary against web https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/ALQ_H.htm
alc.data %>% filter(cycle == "2013-2014") %>% select(alc.value) %>% summary

# check diabetes buckets
gluc1.data %>% select(gluc1.value, gluc1.label) %>% group_by(gluc1.label) %>% 
  summarise(min.gluc = min(gluc1.value), max.gluc = max(gluc1.value))

gluc2.data %>% select(gluc2.value, gluc2.label) %>% group_by(gluc2.label) %>% 
  summarise(min.gluc = min(gluc2.value), max.gluc = max(gluc2.value))

gluc3.data %>% select(gluc3.value, gluc3.label) %>% group_by(gluc3.label) %>% 
  summarise(min.gluc = min(gluc3.value), max.gluc = max(gluc3.value))

# summarize all data
all.data %>% summary
