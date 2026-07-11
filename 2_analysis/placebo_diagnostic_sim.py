import numpy as np, pandas as pd, warnings
warnings.filterwarnings("ignore")
import pyfixest as pf

def build_stacked_panel(n_never=200, n_direct=60, n_indirect=60,
                        storm_years=(2002,2005,2008,2011,2014), pre=4, post=3):
    rows=[]
    directs=[f"D{i}" for i in range(n_direct)]
    indirects=[f"I{i}" for i in range(n_indirect)]
    nevers=[f"N{i}" for i in range(n_never)]
    d_assign={s:[] for s in storm_years}; i_assign={s:[] for s in storm_years}
    for k,s in enumerate(directs):   d_assign[storm_years[k%len(storm_years)]].append(s)
    for k,s in enumerate(indirects): i_assign[storm_years[k%len(storm_years)]].append(s)
    for s in storm_years:
        ss=[(sc,1,0) for sc in d_assign[s]]+[(sc,0,1) for sc in i_assign[s]]+[(sc,0,0) for sc in nevers]
        for (sc,ed,ei) in ss:
            for py in range(s-pre,s+post+1):
                rows.append(dict(sid=s,tea_school_id=sc,panel_year=py,
                                 event_time=py-s,ever_direct=ed,ever_indirect=ei))
    return pd.DataFrame(rows)

stacked=build_stacked_panel()
sch=stacked['tea_school_id'].unique(); yrs=np.arange(1998,2020)
base_fake=pd.DataFrame([(s,y) for s in sch for y in yrs],columns=['tea_school_id','cohort_year'])

levels=[-4,-3,-2,-1,0,1,2,3]; keep=[l for l in levels if l!=-1]  # drop ref -1

def run_once(seed, cluster):
    rng=np.random.default_rng(seed)
    fake=base_fake.copy(); fake['yearly_wages']=rng.normal(size=len(fake))
    df=fake.merge(stacked,left_on=['tea_school_id','cohort_year'],
                  right_on=['tea_school_id','panel_year'],how='inner')
    df['et']=np.where((df.ever_direct==1)|(df.ever_indirect==1),df.event_time,0)
    df['Dlev']=(df.ever_direct*df.et).astype(int)
    df['Ilev']=(df.ever_indirect*df.et).astype(int)
    dcols=[]
    for l in keep:
        c=f"D_{l}"; df[c]=(df.Dlev==l).astype(int); dcols.append(c)
        c=f"I_{l}"; df[c]=(df.Ilev==l).astype(int); dcols.append(c)
    rhs=" + ".join(dcols)
    m=pf.feols(f"yearly_wages ~ {rhs} | tea_school_id + cohort_year",data=df,vcov=cluster)
    return m.tidy()

def monte(cluster,NREP=300,tag=""):
    ests={};rej={}
    for r in range(NREP):
        t=run_once(1000+r,cluster)
        for name,row in t.iterrows():
            ests.setdefault(name,[]).append(row['Estimate'])
            rej.setdefault(name,[]).append(abs(row['t value'])>1.96)
    rs=[]
    for name in ests:
        e=np.array(ests[name]); rs.append((name,e.mean(),e.std(),np.mean(rej[name])))
    sd=pd.DataFrame(rs,columns=['term','mean_est','sd_est','reject_rate']).sort_values('term')
    print(f"\n===== vcov = {tag} =====")
    print(sd.to_string(index=False))
    print("avg |mean_est| =",round(sd.mean_est.abs().mean(),4),
          "| avg reject_rate (want ~0.05) =",round(sd.reject_rate.mean(),3))
    return sd

monte({'CRV1':'tea_school_id'},tag="cluster by school")
monte("iid",tag="iid")
