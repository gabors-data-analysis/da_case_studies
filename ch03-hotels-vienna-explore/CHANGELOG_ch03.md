# Chapter 3 Stata Script Review: Changes for Stata 17+

## File: ch03-hotels-vienna-explore.do → ch03-hotels-vienna-explore_v18.do

**Target Version:** Stata 17+ (for `table`/`collect` system)  
**Maintains backward compatibility:** Stata 15+ (with noted modifications)

### MAJOR UPGRADES

#### 1. **Stata 17+ Table System with Collect** 
**Old (Stata 15):**
```stata
tab city
tab stars
tab city_actual
```

**New (Stata 17+):**
```stata
table (city stars), statistic(frequency) nototals

* To export table to Word/Excel/LaTeX (Stata 17+ only):
* collect style cell, nformat(%9.0fc)
* collect export "${output}/ch03-table-city-stars.docx", replace

* For Stata 15 and below, use:
* tab city stars
```

**Why:**
- Stata 17+ introduced `table` command with `collect` system for publication-quality output
- Can export directly to .docx, .xlsx, .tex, .html, .md, .pdf
- More flexible formatting than old `tab` command
- Backward compatibility maintained with commented alternatives

**Important:** 
- `collect` is for **TABLES**, not graphs/histograms
- Histogram syntax unchanged across Stata versions

---

#### 2. **Version Control & Settings**
**Added:**
```stata
version 18
clear all
set more off
set varabbrev off  // Require full variable names
```

**Why:** 
- Explicit version declaration for reproducibility
- `set varabbrev off` prevents abbreviation errors in shared code
- Better practices for modern Stata

---

#### 3. **Improved File Organization**
**Old:**
```stata
global data_in  "$data_dir/hotels-vienna/clean"
global work  	"ch03-hotels-vienna-explore"
cap mkdir 		"$work/output"
global output 	"$work/output"
```

**New:**
```stata
global data_in  "${data_dir}/hotels-vienna/clean"
global work     "ch03-hotels-vienna-explore"
global output   "${work}/output"

capture mkdir "${work}"
capture mkdir "${output}"
```

**Why:**
- Cleaner variable naming (no tabs)
- Create both directories explicitly
- Consistent use of `${}` syntax

---

#### 4. **Better Data Loading**
**Old:**
```stata
use "$data_in/hotels-vienna.dta", clear
* Or download directly from OSF:
/*
copy "https://osf.io/download/dn8je/" "workfile.dta"
use "workfile.dta", clear
erase "workfile.dta"
*/
```

**New:**
```stata
use "${data_in}/hotels-vienna.dta", clear

* Alternative: Download from OSF (uncomment if needed)
/*
tempfile hotels_data
copy "https://osf.io/download/dn8je/" `hotels_data'
use `hotels_data', clear
*/
```

**Why:**
- Uses `tempfile` instead of creating/deleting workfile.dta
- Cleaner, no file pollution
- More Stata-idiomatic

---

#### 5. **Enhanced Documentation**
**Added:**
- Version stamp: "version 1.0 2025-01-04"
- Stata version requirement note
- Inline backward compatibility comments
- Progress displays throughout
- Section headers for clarity

**Examples added:**
```stata
count
display as text "Sample size: " as result r(N) " hotels"

display as text "Hotels more than 8 miles from center: " as result r(N)

display as text "Final sample size: " as result r(N) " hotels"
```

---

#### 6. **Code Quality Improvements**

**Consistent spacing and alignment:**
- All graph options clearly aligned
- Consistent use of `///` for line continuation
- Removed unnecessary blank lines

**Explicit variable selection:**
- Changed `keep if city== "Vienna"` to `keep if city == "Vienna"` (spacing)
- More readable conditionals

**Local macros for colors:**
```stata
colorpalette viridis, n(4) select(2) nograph
local barcolor `r(p)'

histogram stars, ///
    fcolor(`barcolor') ///
```

---

### STATA 17+ COLLECT SYSTEM EXPLAINED

**What is `collect`?**
The collect system (introduced Stata 17) creates publication-ready tables with:
- Multiple export formats: .docx, .xlsx, .tex, .html, .md, .pdf
- Flexible formatting (decimals, fonts, borders, colors)
- Combined statistics in single table
- Custom labeling and styling

**Example Usage:**
```stata
* Create and style a frequency table
table (city stars), statistic(frequency) nototals
collect style cell, nformat(%9.0fc)
collect label levels city "City Location", modify
collect export "${output}/city_stars_table.docx", replace
```

**What `collect` is NOT for:**
- ❌ Graphs/histograms (use `graph export`)
- ❌ Regression output (use `outreg2`, `estout`, or `etable`)
- ❌ Data manipulation (use regular Stata commands)

**In this script:**
- ✅ Used for: `table` commands showing frequencies
- ❌ NOT used for: histograms (same syntax all versions)

---

### BACKWARD COMPATIBILITY NOTES

Throughout the script, I added notes like:

```stata
* Stata 18+ table command for city_actual
* For Stata 15 and below: use "tab city_actual"
table city_actual, statistic(frequency) nototals
```

This allows users on older versions to easily adapt.

---

### WHAT STAYED THE SAME (Intentionally)

1. **Histogram syntax**: Current approach works well, no breaking changes needed
2. **Color schemes**: viridis palette via colorpalette (user-written package)
3. **Graph export format**: PNG is fine for web
4. **Filtering logic**: No changes to statistical approach
5. **OSF download option**: Already well-implemented

---

### WHAT'S NOT CHANGED (But Could Be in Future)

1. **No ado file created**: Script is short enough (<200 lines)
2. **No putexcel for tables**: Tables in this chapter are simple frequencies
3. **No scheme customization**: Uses colorpalette, which is flexible
4. **No grstyle**: Current approach is adequate

---

### STATA 15 USERS: Quick Adaptation Guide

If using Stata 15, make these changes:

1. **Line 25:** Change `version 18` to `version 15`
2. **Line 27:** Remove `set varabbrev off` (not available in Stata 15)
3. **All `table` commands:** Replace with `tab` or `tabstat`
   - `table (city stars), statistic(frequency) nototals` → `tab city stars`
   - `table city_actual, statistic(frequency) nototals` → `tab city_actual`
4. **Ignore all `collect` export examples** (commented out, so no action needed)

**Everything else works identically in Stata 15!**
- All histograms: ✅ Same syntax
- All graphs: ✅ Same syntax
- All data manipulation: ✅ Same syntax
- OSF download: ✅ Same syntax

---

### TESTING CHECKLIST

- [ ] Script runs on Stata 18 without errors
- [ ] All graphs generate correctly
- [ ] Output directory created successfully  
- [ ] OSF download option works
- [ ] Backward compatibility notes are clear
- [ ] Follows coding standards from R version where applicable

---

### FILES PRODUCED

All in `${output}/`:
1. ch03-figure-1a-hist-stars-Stata.png
2. ch03-figure-1b-hist-stars-Stata.png  
3. ch03-figure-2a-hist-price-Stata.png
4. ch03-figure-2b-hist-price-Stata.png
5. ch03-figure-3a-hist-price-Stata.png
6. ch03-figure-3b-hist-price-Stata.png
7. ch03-figure-4-hist-dist-Stata.png

---

### NEXT STEPS

If this approach is approved:
1. Apply similar upgrades to ch07, ch12, ch22, ch24
2. Consider creating helper ado files for common tasks across chapters
3. Update README with Stata 18 notes
