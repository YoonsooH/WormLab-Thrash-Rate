# Worm Thrashing Rate Calculator

This tool calculates the **thrashing rate (thrashes/minute)** of worms using body bending angle data. It is optimized for **WormLab (MBF Biosciences)** outputs but is adaptable for other datasets meeting the required format.

---

## Installation
* **Requirement:** [R](https://www.r-project.org/) must be installed on your system.
* **Dependencies:** No additional R packages are necessary.
* **Setup:** Download or clone this repository to your local machine.

---

## Usage

1. **Prepare Data:** Place your raw data files in the `Worm_data` directory.
2. **Run Pipeline:** Execute the shell script in your terminal:
   ```bash
   bash wormlabtr.sh
   ```
3. **View Results:** Outputs are automatically stored in the `Worm_output` directory.

### Note for non-WormLab Data
If your data does not originate from WormLab, follow these modifications:
1. Open `wormlabtr.sh` and remove the line: `Rscript R_scripts/BAMP1_Cleaner.R`.
2. Create a directory named `1 Cleaned` inside the `Worm_output` directory.
3. Place your data files directly into `Worm_output/1 Cleaned/`.
4. Run `wormlabtr.sh` as normal.

---

## Raw Data Format

### 1. File Nomenclature
**Crucial:** The filename must follow the exact structure below. **Do not use the underscore (`_`) character elsewhere** in the filename.

**Pattern:** `<EXP GROUP>_<STRAIN>_<REPLICATE NUMBER>_<TIME ID>.csv_Bending Angle - Mid-Point.csv`

**Example:** `M9_VG1038_1_1.csv_Bending Angle - Mid-Point.csv`  
*(Note: The repeated `.csv` is intentional.)*

### 2. File Content
* **WormLab Users:** Do not modify any default configurations when saving the `.csv`. Keep all headers and supplementary information.
* **Non-WormLab Users:** Ensure your `.csv` contains at least these columns:
    * `Frame`: Sequence of frames.
    * `Time`: Timekeeping data.
    * `1`: A "track" column containing numerical data of the body bending angle. (Additional numerical columns for multiple tracks are supported).

---

## Technical Specifications
* **Optimized Frame Rate:** 30 frames per second (fps).
* **Data Duration:** 1-minute worth of data per file.
* **Performance Benchmark:** A dataset of ~875 files (~163.4 MB) typically takes **2 minutes** to process.

---

## Dopaminergic *C. elegans* Mutant Response to Morphine Data

These are data collected for an experiment that investigated the effects of morphine in *cat-2* and *dat-1* loss-of-function *C. elegans* mutants.
* VG1049 corresponds to *cat-2* loss-of-function mutants
* VG1038 corresponds to *dat-1* loss-of-function mutants
* XMN1408 corresponds to wild-type *C. elegans*

For more information, reach out to me: yoonsoo.ham@proton.me or yoonsooham04@gmail.com

Download the data [here](https://osf.io/vekwu/overview?view_only=ef160fafa6214887a3f191d230455bb3). All the .csv files can be placed
in the `Worm_data` directory then the shell script can be executed.

---

> [!TIP]
> Always ensure your terminal is pointed to the root directory of this repository before running the `.sh` script.
