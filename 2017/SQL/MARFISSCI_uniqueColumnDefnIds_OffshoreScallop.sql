/* Pull out unique column definition ids for offshore scallop */

SELECT 
  /* Created by Mike McMahon, 2017/08/16 */
   --md.mon_doc_id, 
   --md.document_no,
   distinct column_defn_id
   FROM marfissci.mon_docs md,
     marfissci.log_efrt_std_info lesi,
     marfissci.log_efrt_entrd_dets dets
WHERE md.mon_doc_defn_id = 15
  AND md.mon_doc_id = lesi.mon_doc_id
  AND lesi.log_efrt_std_info_id =dets.log_efrt_std_info_id
   AND lesi.cdate >= TO_DATE('20080101','YYYYMMDD')
  
  
  
  
  
  