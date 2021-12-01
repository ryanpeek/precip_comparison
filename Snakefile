import pandas as pd
m = pd.read_csv("data_raw/sensor_info_by_id.csv", header=0)
METRICS = m['metric_id'].unique().tolist()
CT_METRICS=[word for word in METRICS if word.startswith("CT")]
#print(CT_METRICS)

#rule all:
#    input: "tst_raw/sensor_info_by_id.csv"    
#    input: 
#        expand("tst_raw/{ct_metrics}.csv.zip", ct_metrics = CT_METRICS) 

rule get_metadata:
    input: 
        script = "code/functions/smk_get_metadata_davis_clim.R"
    output: 
        csv = "tst_raw/sensor_info_by_id.csv"
    conda: "envs/tidyverse.yml"
    shell:"""
    Rscript {input.script}
    """

#rule get_clim_data:
#    input:
#        script = "code/functions/f_download_davis_clim_snake.R",
#        outdir = "tst_raw"
#    output: expand("tst_raw/{ct_metrics}.csv.zip", ct_metrics = CT_METRICS)
#    shell:"""
#    Rscript {input.script} \
#    --outdir {input.outdir}
#    """

rule clean:
    shell:
        "rm -rf tst_raw/*" 
