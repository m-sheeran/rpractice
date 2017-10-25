# Example use: ecomm = add_gps("ecomm 10-04-17.csv", "zip code.csv")
# ecomm_csv and zip_code_csv are filenames provided to function

add_gps = function(ecomm_csv, zip_code_csv)
{
  
  require(data.table)
  require(opsanalysis)
  
  # Read in Ecomm CSV and Zip Code CSV
  ecomm_df = fread(ecomm_csv)
  us_zip_codes = fread(zip_code_csv)
  
  ecomm_df = ecomm_df[ , .(PKT, SHIP_VIA, ZIP_CODE, UNITS, VALUE, WHSE)]
  
  format_col_label(ecomm_df)
  format_col_label(us_zip_codes)
  
  # Add back in leading 0's for Zip Code
  us_zip_codes = us_zip_codes[ , zip_code := formatC(zip_code, width = 5, format = "d", flag = "0")]
  
  # Keep first 5 digits of Zip Code
  ecomm_df = ecomm_df[ , zip_code := substr(zip_code, 1, 5)]
  
  # Key on Zip Code
  setkey(us_zip_codes, zip_code)
  setkey(ecomm_df, zip_code)

  # Left Join to ecomm_df for lat/longs
  ecomm_df = ecomm_df[us_zip_codes, nomatch = "0"]
  return(ecomm_df)
}