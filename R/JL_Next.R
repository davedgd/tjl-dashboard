# 1) implement multi-year data pull (for forecasting); simply need to extend date ranges longer than 1 year to looped single year call

# done, including a month mode (not yet used)

# 2) implement prophet forecasting model

# 3) create graphs by product category

JL_Transactions_V1 %>% select(-starts_with("inclusive_tax")) %>% separate_rows(contains("multival"), sep = ";") %>% select(item_single_qty_money_multival, item_detail_category_name_multival) %>% count(item_detail_category_name_multival)

# 4) create a this year vs. last year plot