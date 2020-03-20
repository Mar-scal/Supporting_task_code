/*select Offshore scallop log marfis data from 2008 on and adds columns based on additional Column definition IDs*/

SELECT 
  /* Modified by J.Sameoto&M.McMahon 2017/08/16; Created by Jerry Black, 2008/03/04 */
   md.mon_doc_id, 
   md.document_no, 
   hi.conf_number,
   md.vr_number,
   hi.hail_in_call_id,
   md.trip_id,
   (SELECT licence_id 
   FROM marfissci.mon_doc_lics 
   WHERE mon_doc_id =  md.mon_doc_id) licence_id,
   --lesi.fv_fished_datetime,
   --lesi.fv_gear_code,
   --lesi.fv_nafo_unit_area_id,
   --lesi.fv_fishing_area_id,
   --lesi.det_nafo_unit_area_id,
   (SELECT area from 
   marfissci.areas 
   WHERE area_id = lesi.det_nafo_unit_area_id) NAFO_Unit_Area,
   --lesi.det_fishing_area_id,
   (SELECT area 
   from marfissci.areas 
   WHERE area_id = lesi.det_fishing_area_id) Fishing_Area,
   lesi.ent_latitude,
   ROUND(TRUNC(lesi.ent_latitude/10000) + MOD(lesi.ent_latitude,10000)/6000.,5) latitude_deg,
   lesi.ent_longitude,
   ROUND(TRUNC(lesi.ent_longitude/10000) + MOD(lesi.ent_longitude,10000)/6000.,5)*-1.0 longitude_deg,
   --lesi.ent_loran_c_chain,
   --lesi.ent_loran_c_x_coord,
   --lesi.ent_loran_c_y_coord,
   (SELECT name FROM marfissci.dmp_companies where lesi.trip_dmp_company_id = dmp_company_id) dmp_company_name,
   (SELECT vessel_name FROM marfissci.vessels WHERE vr_number = md.vr_number) vessel_name, 
   (SELECT TO_DATE(data_value,'YYYY-MON-DD') 
                      FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id =  37) Date_Fished,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 161) AVGSPEED,
      (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 130) FISHING,
         (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 826) DIDNOTFISH,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 172) Watch,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 771) No_Rakes_Fished,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id =  86) No_Tows_per_Watch,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 693) Avg_Tow_Time,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id =  85) Depth_fm,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 192) Bottom_type,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 194) No_of_Bags,
   (SELECT data_value FROM marfissci.log_efrt_entrd_dets WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id AND column_defn_id = 196) Comments,
   lssi.weight,
   (SELECT desc_eng FROM marfissci.specie_size_forms 
     WHERE lssi.ssf_species_code = species_code 
      AND lssi.ssf_species_size_code = species_size_code
      AND lssi.ssf_landed_form_code = landed_form_code) Landed_Form,
   (SELECT desc_eng FROM marfissci.unit_of_measures WHERE lssi.unit_of_measure_id = unit_of_measure_id) Unit_of_Measure,
   ROUND(marfissci.f_round_weight_kgs(lssi.ssf_species_code, lssi.ssf_species_size_code,lssi.ssf_landed_form_code,lssi.weight, lssi.unit_of_measure_id, SYSDATE),3) rw_kgs,
   (SELECT desc_eng FROM marfissci.catch_usages WHERE lssi.catch_usage_code = catch_usage_code) catch_usage,
   (SELECT SUM(RND_WEIGHT_KGS) from marfissci.pro_spc_info WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id and mon_doc_id = md.mon_doc_id) Prorated_Rnd_Weight_kgs,
   (SELECT SUM(RPT_WEIGHT_KGS) from marfissci.pro_spc_info WHERE log_efrt_std_info_id = lesi.log_efrt_std_info_id and mon_doc_id = md.mon_doc_id) Prorated_Rptd_Weight_kgs,
   lssi.post_to_quota_flag,
   (SELECT data_value FROM marfissci.log_spc_entrd_dets WHERE log_spc_std_info_id = lssi.log_spc_std_info_id AND column_defn_id = 824) Roe_On,
   GREATEST(md.cdate,lesi.cdate,lssi.cdate,hi.cdate) greatest_slip_entry_date,
   GREATEST(md.udate,lesi.udate,lssi.udate,hi.udate) greatest_slip_change_date
FROM marfissci.mon_docs md,
     marfissci.log_efrt_std_info lesi,
     marfissci.log_spc_std_info lssi,
    marfissci.hail_in_calls hi
WHERE md.mon_doc_defn_id = 15
  AND md.mon_doc_id = lesi.mon_doc_id
  AND lesi.log_efrt_std_info_id = lssi.log_efrt_std_info_id
  AND md.hail_in_call_id = hi.hail_in_call_id
  AND lesi.cdate >= TO_DATE('20080101','YYYYMMDD')
  
  
  
  
  
  