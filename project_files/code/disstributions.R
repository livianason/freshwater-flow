

#Distribution

sc<-data.frame(raspd = (fal$aspd[fal$site_name=="Spring Creek 10 (Deep)"]),
               rcond = (fal$cond[fal$site_name=="Spring Creek 10 (Deep)"]),
               rpres = (fal$pres[fal$site_name=="Spring Creek 10 (Deep)"]),
               rtemp = (fal$temp[fal$site_name=="Spring Creek 10 (Deep)"]),
               ravdir = (fal$avdir[fal$site_name=="Spring Creek 10 (Deep)"]))
hist(sc$raspd)
hist(sc$rcond,breaks=100)
hist(sc$rpres,breaks=35)
hist(sc$rtemp,breaks=35)
hist(sc$ravdir)


ak<-data.frame(raspd = (fal$aspd[fal$site_name=="AK (Deep)"]),
               rcond = (fal$cond[fal$site_name=="AK (Deep)"]),
               rpres = (fal$pres[fal$site_name=="AK (Deep)"]),
               rtemp = (fal$temp[fal$site_name=="AK (Deep)"]),
               ravdir = (fal$avdir[fal$site_name=="AK (Deep)"]))
hist(ak$raspd)
hist(ak$rcond)
hist(ak$rpres)
hist(ak$rtemp)
hist(ak$ravdir)

rv<-data.frame(raspd = (fal$aspd[fal$site_name=="Revell Sink (Deep)"]),
               rcond = (fal$cond[fal$site_name=="Revell Sink (Deep)"]),
               rpres = (fal$pres[fal$site_name=="Revell Sink (Deep)"]),
               rtemp = (fal$temp[fal$site_name=="Revell Sink (Deep)"]),
               ravdir = (fal$avdir[fal$site_name=="Revell Sink (Deep)"]))
hist(rv$raspd)
hist(rv$rcond)
hist(rv$rpres)
hist(rv$rtemp)
hist(rv$ravdir)
