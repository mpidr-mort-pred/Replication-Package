# Summary of variables kept 

# Mental health

# (r_/s_)rxpsych Takes meds for psych condition (0,1)
# (r_/s_)trpsych Receives psychological treatment (0,1)
# (r_/s_)traumatic_events summ child death, illness, spouse illness, nat. disaster, parent drunk, spouse drunk, physical attack, gun (0-8)

# Welfare 

# (r_/s_)lifeinv Value life insurance policies (>= 0, continuous)
# (r_/s_)nlfins Number life insurance policies (>= 0, continuous integer)
# (r_/s_)witwill Has a witnessed will (0,1)
# (r_/s_)wlifein Has whole life insurance (0,1)
# (r_/s_)wtrust  Has a trust (0,1)
# (r_)lifeins_fam Whether a family member is beneficiary of life ins (0,1)

# Support 

# (r_/s_)ftrhlp Anyone able to help r with future adl needs (0,1)
# (r_/s_)mealhlp Whether anyone helps r with meal preparation (0,1)
# (r_/s_)medhlp Whether anyone helps r with taking medication (0,1)
# (r_/s_)moneyhlp Whether anyone helps r with managing money (0,1)
# (r_/s_)phonehlp Whether anyone helps r with phone calls (0,1)
# (r_/s_)shophlp Whether anyone helps r with grocery shopping (0,1)
# (r_/s_)rcany Receives any care for adls/iadls (0,1)
# (r_)adl_hour_fam Hours/day family members help r with adls/iadls (>=0, continuous integer)
# (r_)adl_fam # family members help r with adls/iadls (>=0, continuous integer)
# (r_)adl_hour_nf Hours/day non family members help r with adls/iadls (>=0, continuous integer)
# (r_)adl_nf # non family members help r with adls/iadls (>=0, continuous integer)

# Longitudinal variable list 


# (s_)agey_b age in years at interview
# (r_/s_)bcohort Cohort of birth 
# (r_/s_)gender
# (r_/s_)race race non-hispanic white, non-hispanic black, hispanic, other
# (r_/s_)edyrs Years of education
# (r_/s_)meduc Mother's Years of education 
# (r_/s_)feduc Father's years of education 
# (r_)mstat Marital Status
# (r_/s_)mrct Number of marriages 
# (r_/s_)mdiv Number of divorces 
# (r_/s_)everdiv Ever divorced
# (r_/s_)everwid Ever widowed
# (r_)evermrg Ever married
# (r_/s_)relig Religion
# (r_/s_)vetrn Veteran Status
# (r_/s_)bplace Place of Birth (Cens Region)
# (r_/s_)momliv Mother alive
# (r_/s_)dadliv Father alive
# (r_/s_)momage Mother age current/at death
# (r_/s_)dadage Father age current/at death
# (h_)hhres Number of people in HH
# (h_)child Number of living children
# (r_/s_)evbrn Number of Children Ever Born
# (r_/s_)smokev Ever smoked cigarettes


# Childhood 

# (r_/s_)lhchild	summary count for life history childhood stress items: divorced before 16, difficult living arrangement, death of a parent before 16 (0-3)
# (r_/s_)dadgrela	before age 18 r had a good relationship with father (1-5)
# (r_/s_)pabused	before age 18 r was physically abused by parent (0,1)
# (r_/s_)financh	financial condition while was growing up (1-3)
# (r_/s_)chshlt	childhood health status (1-5)
# (r_/s_)dadoccup	father's occupation at age 16 (factor 0-3)


# Habits

# (r_/s_)cage	cage summary: drinkcut + drinkcr + drinkbd + drinknr (0-4)
# (r_/s_)smokef # cigarettes/day (continuous)
# (r_/s_)quitsmok	r age quit smoking (continuous) 
# (r_/s_)strtsmok	r age started smoking (continuous) 
# (r_/s_)binged	r number days binge drinks (continuous) 


# Demographic

# (h_)lvwith	living arrangement (1-5) 
# (r_)hometyp	type of home r lives in (factor 1-7)
# (h_)rural	lives in rural or urban (0,1)
# (h_)kidu14	number children/grandchildren in hh under age 14 (0-8) 
# (h_)kidu6	number children/grandchildren in hh under age 6 (0-5)
# (h_)depndntn	number dependents (continuous)
# (h_)grchild	number of grandchildren (continuous) 
# (r_/s_)arriage	r age started to live in U.S. (continuous)
# (r_/s_)educl	r harmonized education level (1-3)
# (r_/s_)mcrtyr	r year of current marriage (continuous) 
# (r_/s_)citizen	whether s has U.S. citizenship (0,1) 

# Physical Health

# (r_/s_)mobese	r whether measured obese (0,1)
# (r_/s_)balance	r balance test summary score (1-4)
# (r_/s_)sum_med summary of all rxs added together (continuous)
# (r_/s_)sbreath	frequency r short of breath (1-4) 
# (r_/s_)adlfive	r 5-item adl summary/0-5:  In Wave 1, ADLFIVE is the sum of BATH + DRESS + EAT + BED + v9034 (0-5)
# (r_/s_)lstmnspd	r age last menstrual period (continuous) 
# (r_/s_)reccancr	r age most recent cancer diagnosis (continuous)
# (r_/s_)rechrtatt	r age most recent heart attack (continuous) 
# (r_/s_)recstrok	r age most recent stroke (continuous) 
# (r_/s_)urinai	r any urinary incontinence (0,1)
# (r_/s_)diasto	r average blood pressure measure (diastolic) 2 & 3 (continuous) 
# (r_/s_)systo	r average blood pressure measure (systolic) 2 & 3 (continuous) 
# (r_/s_)pulse	r average pulse measure 2 & 3 (continuous) 
# (r_/s_)cancrst	r cancer status (1-3) 
# (r_/s_)hipe	r ever fractured hip (0,1) 
# (r_/s_)hrtrhme	r ever had abnormal heart rhythm (0,1) 
# (r_/s_)angine	r ever had angina (0,1) 
# (r_/s_)cancre	r ever had cancer (0,1) 
# (r_/s_)diabe	r ever had diabetes (0,1) 
# (r_/s_)catrcte	r ever had cataract surgery (0,1) 
# (r_/s_)conhrtfe	r ever had congestive heart failure (0,1) 
# (r_/s_)hrtatte	r ever had heart attack (0,1)
# (r_/s_)stroke	r ever had heart stroke
# (r_/s_)hrtsrge	r ever had heart surgery
# (r_/s_)hchole	r ever had high cholesterol (0,1)
# (r_/s_)hystere	r ever had hysterectomy (0,1) 
# (r_/s_)jointre	r ever had joint replaced (0,1)
# (r_/s_)osteoe	r ever had osteoporosis (0,1)
# (r_/s_)shingle	r ever had shingles (0,1)
# (r_/s_)glaucoma	r ever treated for glaucoma (0,1) 
# (r_/s_)lowermob	r lower body mobility summary:  In Wave 1, LOWERMOB is the sum of WALK1 + CLIMS + CHAIR (0-4)
# (r_/s_)ncatrct	r number eyes cataract surg this wave (1,2) 
# (r_/s_)pneushte	r pneumonia vaccine ever (0,1)
# (r_/s_)hrtrhm	r reports abnormal heart rhythm since last wave (0,1)
# (r_/s_)catrct	r reports cataract surgery this wave (0,1)
# (r_/s_)conhrtf	r reports congestive heart failure since last wave (0,1)
# (r_/s_)hrtatt	r reports heart attack since last wave (0,1) 
# (r_/s_)sight	r self-rated eyesight (1-6)
# (r_/s_)hearing	r self-rated hearing (1-5)
# (r_/s_)fatigue	r severe fatigue (0,1)
# (r_/s_)shnglshte	r shingles vaccine ever (0,1)
# (r_/s_)uppermob	r upper body mobility summary: In Wave 1, UPPERMOB is the sum of ARMS + LIFT + DIME (0-3)
# (r_/s_)painlv	r usual level of pain (0-3)
# (r_/s_)lunglmt	r whether lung condition limits activity (0,1)
# (r_/s_)diaghrtr	s age first diagnosed with abnormal heart rhythm (continuous)
# (r_/s_)diagangin	s age first diagnosed with angina (continuous)
# (r_/s_)diagchf	s age first diagnosed with congestive heart failure (continuous)
# (r_/s_)diagdiab	s age first diagnosed with diabetes (continuous)
# (r_/s_)frhrtatt	s age first heart attack (continuous) 
# (r_/s_)limimpar	whether r limited in any way due to impairment (0,1)

# Job

# (r_/s_)csize Size of total company/org (0-1.000.000, continuous)
# (r_/s_)jdealpplb Freq cur job dealing with people (1-4)
# (r_/s_)jdiffa Cur job more difficult than used to be (1-4)
# (r_/s_)jenjwrka Really enjoys going to work (1-4)
# (r_/s_)jobsum	Job stress summary mean score (1-4, continuous)
# (r_/s_)jpdysa Cur job paid days off (0-440, continuous)
# (r_/s_)jrsleft Reason for stopping working (if not working) (1-8)
# (r_/s_)jsprvsn Number people supervises (0-1.000, continuous)
# (r_/s_)lookwrkpf Look part or full-time job (if not working) (1-4)
# (r_/s_)satjob Whether is satisfied with their job (1-4)
# (r_/s_)wdiscrim Job discrimination score (2 - 28.3, continuous)
# (r_/s_)jobsum2 Job stress summary score (4.16 - 36.5, continuous)
# (r_/s_)work Currently working for pay (0,1)
# (r_/s_)unemp Unemployed (0,1)
# (r_/s_)jhours Hours of work per week at current job (0-98, continuous)

# Social

# (r_/s_)dcsxori Discrimination reason:sexual orientation (0,1)
# (r_/s_)dscrim	6 discrimination summary mean score (1-6, continuous)
# (r_/s_)fsupport	Lack of friends support summary mean score: (1-4, continuous)
# (h_)gcaany Provides any informal care (0,1)
# (r_/s_/h_)kcnt Any weekly contact with children in person/phone/email (0,1)
# (r_/s_)ksupport	Lack of children support summary mean score: (1-4, continuous)
# (r_/s_)npdisum	Neighborhood physical disorder summary mean score (1-7, continuous)
# (r_/s_)nsocosum	Neighborhood social cohesion summary mean score (1-7, continuous)
# (r_/s_)osupport	Lack of other family members support summary mean score (1-4, continuous)
# (h_)pcnt Any weekly contact with parents in person/phone/email  (0,1)
# (r_/s_)relgwk Any weekly part. in religious services  (0,1)
# (r_/s_)rfcnt Any weekly contact with relative/friend in person/phone/mail/email  (0,1)
# (r_/s_)socwk Any weekly social activities  (0,1)
# (r_/s_)unfair Lifetime unfair experiences count (0-7)
# (r_/s_)ssupport Lack of r spouse support summary mean score (1-4, continuous)  

# Wealth

# (h_)adebt	Assets:debts [not yet asked]--cross-wave (0-6.666.733, continuous)
# (h_)itot	Incm: total hhold / r+sp only (0-60.014.376, continuous)
# (h_)atotb_us Total assets US (-4.383.000 - 25.900.000, continuous)

