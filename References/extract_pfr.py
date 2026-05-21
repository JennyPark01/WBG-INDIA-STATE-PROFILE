import pyxlsb
import csv

wb = pyxlsb.open_workbook("C:/Users/jaeye/Downloads/India PFR Tool.xlsb")

# Get state names
state_names = []
with wb.get_sheet("_States") as sheet:
    for i, row in enumerate(sheet.rows()):
        if i == 0:
            continue
        name = row[0].v if row[0].v else None
        if name:
            state_names.append(name)
        else:
            break

# Raw variable codes to extract
raw_codes = [
    "NGSDP", "realgsdp", "realpcnsdp", "CPI",
    "REV_TOTAL", "EXP_CUR_TOTAL", "EXP_CUR_II_C_IP",
    "EXP_CAP_I", "EXP_CAP_IV", "TOTLIAB", "POV", "cvi",
    "nomagriculture", "industry", "nomservices_tr",
]

# Read header for year indices
headers = None
year_indices = {}
with wb.get_sheet("_Data") as sheet:
    for i, row in enumerate(sheet.rows()):
        if i == 0:
            headers = [c.v for c in row]
            for j, h in enumerate(headers):
                if h is not None:
                    try:
                        yr = int(float(str(h)))
                        if 1989 <= yr <= 2024:
                            year_indices[j] = yr
                    except:
                        pass
            break

print(f"Year columns: {sorted(year_indices.values())}")

# Extract raw data: state -> code -> {year: value}
raw_data = {s: {} for s in state_names}
with wb.get_sheet("_Data") as sheet:
    for i, row in enumerate(sheet.rows()):
        if i == 0:
            continue
        key = str(row[0].v) if row[0].v else ""
        for state in state_names:
            for code in raw_codes:
                if key == state + code + "None":
                    raw_data[state][code] = {}
                    for j, yr in year_indices.items():
                        if j < len(row) and row[j].v is not None:
                            try:
                                raw_data[state][code][yr] = float(row[j].v)
                            except:
                                pass

# Compute derived metrics
output_rows = []
years = sorted([y for y in year_indices.values() if y >= 1990])

for state in state_names:
    d = raw_data[state]

    for yr in years:
        # Growth of real per capita income
        if "realpcnsdp" in d and yr in d["realpcnsdp"] and (yr - 1) in d["realpcnsdp"]:
            prev = d["realpcnsdp"][yr - 1]
            curr = d["realpcnsdp"][yr]
            if prev != 0:
                growth = (curr - prev) / prev * 100
                output_rows.append([state, "growth_pcgdp", "Growth of real per capita income", yr, round(growth, 2)])

        # GDP growth
        if "realgsdp" in d and yr in d["realgsdp"] and (yr - 1) in d["realgsdp"]:
            prev = d["realgsdp"][yr - 1]
            curr = d["realgsdp"][yr]
            if prev != 0:
                growth = (curr - prev) / prev * 100
                output_rows.append([state, "gdp_growth", "GDP growth, percent", yr, round(growth, 2)])

        # Sector decomposition (pp contributions to GDP growth)
        if "realgsdp" in d and yr in d["realgsdp"] and (yr - 1) in d["realgsdp"]:
            prev_gdp = d["realgsdp"][yr - 1]
            if prev_gdp != 0:
                for sec_code, sec_label in [("nomagriculture", "Agriculture"), ("industry", "Industry"), ("nomservices_tr", "Services")]:
                    if sec_code in d and yr in d[sec_code] and (yr - 1) in d[sec_code]:
                        contrib = (d[sec_code][yr] - d[sec_code][yr - 1]) / prev_gdp * 100
                        output_rows.append([state, "sector_" + sec_label.lower(), sec_label, yr, round(contrib, 2)])
                # Other = GDP growth - sum of sectors
                gdp_gr = (d["realgsdp"][yr] - prev_gdp) / prev_gdp * 100
                sec_sum = 0
                for sec_code in ["nomagriculture", "industry", "nomservices_tr"]:
                    if sec_code in d and yr in d[sec_code] and (yr - 1) in d[sec_code]:
                        sec_sum += (d[sec_code][yr] - d[sec_code][yr - 1]) / prev_gdp * 100
                other = gdp_gr - sec_sum
                output_rows.append([state, "sector_other", "Other", yr, round(other, 2)])

        # Inflation (CPI growth)
        if "CPI" in d and yr in d["CPI"] and (yr - 1) in d["CPI"]:
            prev = d["CPI"][yr - 1]
            curr = d["CPI"][yr]
            if prev != 0:
                infl = (curr - prev) / prev * 100
                output_rows.append([state, "inflation", "Inflation", yr, round(infl, 2)])

        # GDP per capita (thousand INR) – matches 1_1 sheet: realgsdp / 1000
        if "realgsdp" in d and yr in d["realgsdp"]:
            output_rows.append([state, "gdp_percapita", "GDP per capita, thousand INR", yr, round(d["realgsdp"][yr] / 1000, 2)])

        ngdp = d.get("NGSDP", {}).get(yr, None)
        if ngdp and ngdp != 0:
            # Fiscal revenues % GDP
            if "REV_TOTAL" in d and yr in d["REV_TOTAL"]:
                val = d["REV_TOTAL"][yr] / ngdp * 100
                output_rows.append([state, "rev_pct_gdp", "Fiscal revenues, pct of GDP", yr, round(val, 2)])

            # Current expenditure % GDP
            if "EXP_CUR_TOTAL" in d and yr in d["EXP_CUR_TOTAL"]:
                val = d["EXP_CUR_TOTAL"][yr] / ngdp * 100
                output_rows.append([state, "cur_exp_pct_gdp", "Total current expenditure", yr, round(val, 2)])

            # Interest payments % GDP
            if "EXP_CUR_II_C_IP" in d and yr in d["EXP_CUR_II_C_IP"]:
                val = d["EXP_CUR_II_C_IP"][yr] / ngdp * 100
                output_rows.append([state, "interest_pct_gdp", "Interest payments", yr, round(val, 2)])

            # Capital outlay % GDP
            if "EXP_CAP_I" in d and yr in d["EXP_CAP_I"]:
                val = d["EXP_CAP_I"][yr] / ngdp * 100
                output_rows.append([state, "capex_pct_gdp", "Capital Outlay", yr, round(val, 2)])

            # Loans % GDP
            if "EXP_CAP_IV" in d and yr in d["EXP_CAP_IV"]:
                val = d["EXP_CAP_IV"][yr] / ngdp * 100
                output_rows.append([state, "loans_pct_gdp", "Loans and Advances", yr, round(val, 2)])

            # Total expenditures % GDP
            cur = d.get("EXP_CUR_TOTAL", {}).get(yr, 0)
            cap1 = d.get("EXP_CAP_I", {}).get(yr, 0)
            cap4 = d.get("EXP_CAP_IV", {}).get(yr, 0)
            total_exp = cur + cap1 + cap4
            val = total_exp / ngdp * 100
            output_rows.append([state, "total_exp_pct_gdp", "Total expenditures", yr, round(val, 2)])

            # Fiscal balance % GDP
            rev = d.get("REV_TOTAL", {}).get(yr, 0)
            fb = (rev - total_exp) / ngdp * 100
            output_rows.append([state, "fiscal_balance", "Fiscal balance", yr, round(fb, 2)])

            # Primary balance = fiscal balance + interest payments
            ip = d.get("EXP_CUR_II_C_IP", {}).get(yr, 0)
            pb = fb + ip / ngdp * 100
            output_rows.append([state, "primary_balance", "Primary balance", yr, round(pb, 2)])

            # Total liabilities % GDP
            if "TOTLIAB" in d and yr in d["TOTLIAB"]:
                val = d["TOTLIAB"][yr] / ngdp * 100
                output_rows.append([state, "liab_pct_gdp", "Total liabilities", yr, round(val, 2)])

        # Poverty index (raw value)
        if "POV" in d and yr in d["POV"]:
            output_rows.append([state, "pov_index", "Multilateral poverty index", yr, round(d["POV"][yr], 2)])

        # CVI (raw value)
        if "cvi" in d and yr in d["cvi"]:
            output_rows.append([state, "cvi", "Climate Vulnerability Index", yr, round(d["cvi"][yr], 2)])

# Write CSV
csv_path = "C:/Users/jaeye/Downloads/State profile/pfr_1_1_data.csv"
with open(csv_path, "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(["State", "Code", "Indicator", "Year", "Value"])
    writer.writerows(output_rows)

print(f"Written {len(output_rows)} rows to {csv_path}")

# Verify Bihar values
for r in output_rows:
    if r[0] == "Bihar" and r[1] == "rev_pct_gdp" and r[3] == 2017:
        print(f"Bihar rev_pct_gdp 2017: {r[4]} (expected ~25.06)")
    if r[0] == "Bihar" and r[1] == "total_exp_pct_gdp" and r[3] == 2017:
        print(f"Bihar total_exp_pct_gdp 2017: {r[4]} (expected ~28.11)")
    if r[0] == "Bihar" and r[1] == "fiscal_balance" and r[3] == 2017:
        print(f"Bihar fiscal_balance 2017: {r[4]} (expected ~-3.05)")

from collections import Counter
code_counts = Counter(r[1] for r in output_rows)
print(f"\nTotal indicators: {len(code_counts)}")
for code, count in sorted(code_counts.items()):
    print(f"  {code}: {count} rows")

wb.close()
