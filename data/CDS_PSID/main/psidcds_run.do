
clear all
set more off

cd "C:\Users\S6150145\Dropbox\irina_jmp\ReplicationFiles\data\schools_cds\main"


quietly do cds_psid_org


sort pid year

count

save datasets/cds_final, replace

