import streamlit as st
import subprocess
import os
import glob
import zipfile
import io
import shutil
import time

st.set_page_config(
    page_title="Georgia Southern Postgame Hitting Reports",
    page_icon="⚾",
    layout="centered"
)

NAVY = "#011E41"

st.markdown(
    f"""
    <style>
        .stApp {{
            background-color: white;
        }}

        h1 {{
            color: {NAVY};
            font-weight: 800;
        }}

        .stButton > button {{
            background-color: {NAVY};
            color: white;
            border: none;
            font-weight: 700;
        }}

        .stDownloadButton > button {{
            border-color: {NAVY};
            color: {NAVY};
            font-weight: 700;
        }}
    </style>
    """,
    unsafe_allow_html=True
)

st.title("Georgia Southern Postgame Hitting Reports")

st.write(
    "Upload a raw TrackMan game CSV and automatically generate "
    "the postgame hitting report for every Georgia Southern hitter."
)

uploaded_file = st.file_uploader(
    "TrackMan CSV",
    type=["csv"]
)

if uploaded_file is not None:

    st.success(f"Loaded: {uploaded_file.name}")

    if st.button("Generate Reports", type="primary"):

        with st.spinner("Generating hitter reports..."):

            try:
                # -------------------------------------------------
                # 1. Save uploaded TrackMan file
                # -------------------------------------------------

                csv_path = "sample_game.csv"

                # Backup the developer's original sample CSV
                backup_path = None

                if os.path.exists(csv_path):
                    backup_path = "sample_game_backup.csv"
                    shutil.copy2(csv_path, backup_path)

                with open(csv_path, "wb") as f:
                    f.write(uploaded_file.getbuffer())

                # -------------------------------------------------
                # 2. Snapshot existing PDFs
                # -------------------------------------------------

                before = {
                    os.path.abspath(f): os.path.getmtime(f)
                    for f in glob.glob("*.pdf")
                }

                run_started = time.time()

                # -------------------------------------------------
                # 3. Run the exact R report
                # -------------------------------------------------

                env = os.environ.copy()
                env["R_LIBS_USER"] = os.path.expanduser("~/R/library")

                result = subprocess.run(
                    ["Rscript", "hitter_report.R"],
                    capture_output=True,
                    text=True,
                    env=env,
                    cwd=os.getcwd()
                )

                # -------------------------------------------------
                # 4. Restore original sample CSV
                # -------------------------------------------------

                if backup_path and os.path.exists(backup_path):
                    shutil.move(backup_path, csv_path)

                # -------------------------------------------------
                # 5. Handle R errors
                # -------------------------------------------------

                if result.returncode != 0:

                    st.error("The report generator encountered an error.")

                    with st.expander("Show error details"):
                        st.code(result.stdout + "\n" + result.stderr)

                    st.stop()

                # -------------------------------------------------
                # 6. Find PDFs created/updated by THIS run
                # -------------------------------------------------

                generated_pdfs = []

                for pdf in glob.glob("*.pdf"):

                    abs_pdf = os.path.abspath(pdf)
                    modified = os.path.getmtime(pdf)

                    old_modified = before.get(abs_pdf)

                    if old_modified is None or modified >= run_started - 1:
                        generated_pdfs.append(pdf)

                generated_pdfs.sort()

                if not generated_pdfs:
                    st.warning(
                        "The R script finished, but no new PDF files were found."
                    )
                    st.stop()

                # -------------------------------------------------
                # 7. Store PDFs in Streamlit session
                # -------------------------------------------------

                st.session_state["reports"] = {}

                for pdf in generated_pdfs:
                    with open(pdf, "rb") as f:
                        st.session_state["reports"][pdf] = f.read()

                st.session_state["r_output"] = result.stdout

                st.success(
                    f"Reports generated successfully — "
                    f"{len(generated_pdfs)} PDF files created."
                )

            except Exception as e:

                st.error("Something went wrong while generating the reports.")
                st.exception(e)


# -------------------------------------------------
# DOWNLOAD SECTION
# -------------------------------------------------

if "reports" in st.session_state:

    reports = st.session_state["reports"]

    st.divider()

    st.header("Download Reports")

    # ---------------------------------------------
    # ZIP containing everything
    # ---------------------------------------------

    zip_buffer = io.BytesIO()

    with zipfile.ZipFile(
        zip_buffer,
        "w",
        zipfile.ZIP_DEFLATED
    ) as z:

        for filename, data in reports.items():
            z.writestr(filename, data)

    st.download_button(
        "⬇ Download All Reports",
        data=zip_buffer.getvalue(),
        file_name="GS_Postgame_Hitting_Reports.zip",
        mime="application/zip",
        use_container_width=True
    )

    # ---------------------------------------------
    # Combined report
    # ---------------------------------------------

    combined = [
        name for name in reports
        if "_Hitter_Reports_" in name
        and "_Hitter_Report_" not in name
    ]

    if combined:

        st.subheader("Combined Team Report")

        for name in combined:

            st.download_button(
                f"Download {name}",
                data=reports[name],
                file_name=name,
                mime="application/pdf",
                key=f"combined_{name}",
                use_container_width=True
            )

    # ---------------------------------------------
    # Individual reports
    # ---------------------------------------------

    individual = [
        name for name in reports
        if "_Hitter_Report_" in name
    ]

    if individual:

        st.subheader("Individual Hitter Reports")

        for name in individual:

            player_name = (
                name.split("_Hitter_Report_")[0]
                .replace("_", " ")
            )

            st.download_button(
                player_name,
                data=reports[name],
                file_name=name,
                mime="application/pdf",
                key=f"player_{name}",
                use_container_width=True
            )
            