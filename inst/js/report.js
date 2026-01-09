
/* ============================================================================
   OMOP Delivery Report - Interactive JavaScript
   ============================================================================ */

// Global state
let currentGroup = "Clinical Data";
let currentTable = null;
let savedScrollPosition = 0;

// ============================================================================
// TABLE GROUP SWITCHING
// ============================================================================

function switchTableGroup(groupName) {
  console.log("=== switchTableGroup called ===");
  console.log("Group name:", groupName);

  // Hide all group content
  const allGroups = document.querySelectorAll(".table-group-content");
  console.log("Found", allGroups.length, "group elements");

  allGroups.forEach((group, index) => {
    console.log("  Hiding group " + index + ":", group.id);
    group.style.display = "none";
  });

  // Show selected group
  const groupId = "group-" + groupName.toLowerCase().replace(/ /g, "-");
  console.log("Looking for group ID:", groupId);

  const selectedGroup = document.getElementById(groupId);
  if (selectedGroup) {
    selectedGroup.style.display = "block";
    console.log("Successfully showed group:", groupId);
  } else {
    console.error("Group not found:", groupId);
  }

  currentGroup = groupName;

  // Update type concepts for this group
  updateGroupTypeConcepts(groupName);

  console.log("Current group set to:", currentGroup);
  console.log("=== switchTableGroup complete ===");
}

// ============================================================================
// TABLE SORTING
// ============================================================================

var sortStates = {};

function sortDeliveryTable(groupId, columnIndex, sortType) {
  var tableId = "delivery-table-" + groupId;
  var tbodyId = "delivery-tbody-" + groupId;
  var tbody = document.getElementById(tbodyId);
  var table = document.getElementById(tableId);

  if (!tbody || !table) return;

  // Get current sort state for this table and column
  var stateKey = tableId + "-" + columnIndex;
  var currentSort = sortStates[stateKey] || "none";

  // Toggle sort direction
  var newSort = "asc";
  if (currentSort === "asc") {
    newSort = "desc";
  }

  sortStates[stateKey] = newSort;

  // Remove sort classes from all headers in this table
  var headers = table.querySelectorAll("th.sortable");
  for (var i = 0; i < headers.length; i++) {
    headers[i].classList.remove("sort-asc", "sort-desc");
  }

  // Add sort class to clicked header
  headers[columnIndex > 1 ? columnIndex - 1 : columnIndex].classList.add("sort-" + newSort);

  // Get all rows and convert to array
  var rows = Array.from(tbody.querySelectorAll("tr"));

  // Sort rows
  rows.sort(function(a, b) {
    var aText = a.cells[columnIndex].textContent.trim();
    var bText = b.cells[columnIndex].textContent.trim();

    var aVal, bVal;

    if (sortType === "number") {
      // Remove commas and parse as number
      aVal = parseFloat(aText.replace(/,/g, "")) || 0;
      bVal = parseFloat(bText.replace(/,/g, "")) || 0;
    } else {
      // Text comparison (case insensitive)
      aVal = aText.toLowerCase();
      bVal = bText.toLowerCase();
    }

    if (newSort === "asc") {
      return aVal > bVal ? 1 : aVal < bVal ? -1 : 0;
    } else {
      return aVal < bVal ? 1 : aVal > bVal ? -1 : 0;
    }
  });

  // Re-append sorted rows
  for (var i = 0; i < rows.length; i++) {
    tbody.appendChild(rows[i]);
  }
}

// ============================================================================
// TABLE DRILLDOWN
// ============================================================================

function showTableDrilldown(tableName) {
  currentTable = tableName;

  // Save current scroll position
  savedScrollPosition = window.pageYOffset || document.documentElement.scrollTop;

  // Hide sidebar
  const sidebar = document.querySelector(".sidebar");
  if (sidebar) {
    sidebar.style.display = "none";
  }

  // Hide all other sections
  const sectionsToHide = ["overview", "dqd-grid", "delivery-report", "time-series", "vocab-harmonization", "technical-summary"];
  sectionsToHide.forEach(function(sectionId) {
    const section = document.getElementById(sectionId);
    if (section) {
      section.style.display = "none";
    }
  });

  // Show drilldown section
  const drilldownSection = document.getElementById("table-drilldown");
  drilldownSection.style.display = "block";

  // Update title with table name
  document.getElementById("drilldown-table-name").textContent = tableName;

  // Get table data from embedded data
  const tableData = getTableData(tableName);

  // Build drilldown content
  const content = buildTableDrilldownContent(tableData);
  document.getElementById("drilldown-content").innerHTML = content;

  // Initialize table drilldown timeline
  initializeTableDrilldownTimeSeries(tableName);

  // Scroll to top of page
  window.scrollTo(0, 0);

  // Add history state so browser back button works
  history.pushState({ view: "drilldown", table: tableName }, "", "#" + tableName);

  console.log("Showing drilldown for table:", tableName);
}

function hideTableDrilldown() {
  // Show sidebar
  const sidebar = document.querySelector(".sidebar");
  if (sidebar) {
    sidebar.style.display = "block";
  }

  // Show all other sections
  const sectionsToShow = ["overview", "dqd-grid", "delivery-report", "time-series", "vocab-harmonization", "technical-summary"];
  sectionsToShow.forEach(function(sectionId) {
    const section = document.getElementById(sectionId);
    if (section) {
      section.style.display = "block";
    }
  });

  // Hide drilldown section
  document.getElementById("table-drilldown").style.display = "none";

  currentTable = null;

  // Restore scroll position after DOM has re-rendered
  // Use setTimeout to ensure the browser has finished updating the layout
  setTimeout(function() {
    window.scrollTo(0, savedScrollPosition);
  }, 0);

  // Update URL to remove table anchor
  if (window.location.hash) {
    history.pushState({ view: "main" }, "", window.location.pathname);
  }

  console.log("Hiding table drilldown");
}

// ============================================================================
// DATA RETRIEVAL HELPERS
// ============================================================================

function getTableData(tableName) {
  // Find table in any group
  for (const groupName in REPORT_DATA.groups) {
    const group = REPORT_DATA.groups[groupName];
    if (group.tables && group.tables[tableName]) {
      return group.tables[tableName];
    }
  }
  return null;
}

function getGroupData(groupName) {
  return REPORT_DATA.groups[groupName] || null;
}

// ============================================================================
// TABLE DRILLDOWN CONTENT BUILDER
// ============================================================================

function buildTableDrilldownContent(tableData) {
  if (!tableData) {
    return "<p>No data available for this table.</p>";
  }

  let html = "";

  // Get pre-computed quality metrics from R
  var defaultDateRows = tableData.default_date_rows || 0;
  var defaultDatePercent = (tableData.default_date_percent || 0).toFixed(1);
  var invalidConceptRows = tableData.invalid_concept_rows || 0;
  var invalidConceptPercent = (tableData.invalid_concept_percent || 0).toFixed(1);

  // Build consolidated Data Quality Alerts section
  var qualityWarnings = [];

  // Vocabulary tables (skip default date warnings for these)
  var vocabularyTables = [
    "concept", "vocabulary", "domain", "concept_class",
    "concept_relationship", "relationship", "concept_synonym",
    "concept_ancestor", "source_to_concept_map", "drug_strength"
  ];
  var isVocabularyTable = vocabularyTables.indexOf(tableData.name) !== -1;

  // Count validation warning
  if (tableData.counts_valid === false) {
    qualityWarnings.push(`🧮 <strong>Row count mismatch:</strong> Expected final rows: ` + formatNumber(tableData.expected_final_rows) + `, Actual: ` + formatNumber(tableData.final_rows) + `. Please review the pipeline output.`);
  }

  // Default dates warning (>1%) - skip for vocabulary tables
  if (parseFloat(defaultDatePercent) > 1 && tableData.final_rows > 0 && !isVocabularyTable) {
    qualityWarnings.push(`📅 <strong>` + defaultDatePercent + `%</strong> of rows have default/placeholder dates`);
  }

  // Invalid concepts warning (>0)
  if (invalidConceptRows > 0 && tableData.final_rows > 0) {
    var rowWord = invalidConceptRows === 1 ? "row has" : "rows have";
    qualityWarnings.push(`📖 <strong>` + formatNumber(invalidConceptRows) + `</strong> ` + rowWord + ` invalid concept IDs`);
  }

  // Invalid rows warning (>0)
  if (tableData.invalid_rows > 0 && tableData.final_rows > 0) {
    var invalidRowWord = tableData.invalid_rows === 1 ? "row does" : "rows do";
    var invalidRemovedWord = tableData.invalid_rows === 1 ? "was removed" : "were removed";
    qualityWarnings.push(`🧩 <strong>` + formatNumber(tableData.invalid_rows) + `</strong> ` + invalidRowWord + ` not meet OMOP data type specifications and ` + invalidRemovedWord);
  }

  // Missing Connect ID warning (>0)
  if (tableData.missing_person_id_rows > 0 && tableData.final_rows > 0) {
    var missingRowWord = tableData.missing_person_id_rows === 1 ? "row does" : "rows do";
    var missingRemovedWord = tableData.missing_person_id_rows === 1 ? "was removed" : "were removed";
    qualityWarnings.push(`👤 <strong>` + formatNumber(tableData.missing_person_id_rows) + `</strong> ` + missingRowWord + ` not have a Connect ID value and ` + missingRemovedWord);
  }

  // Referential integrity violations warning (>0)
  if (tableData.referential_integrity_violations > 0 && tableData.final_rows > 0) {
    var violationWord = tableData.referential_integrity_violations === 1 ? "row has a person_id" : "rows have person_ids";
    qualityWarnings.push(`🧑‍🧒 <strong>` + formatNumber(tableData.referential_integrity_violations) + `</strong> ` + violationWord + ` that do not exist in the person table`);
  }

  // Display warnings if any exist
  if (qualityWarnings.length > 0) {
    html += `
      <div class="info-box drilldown-alert-box">
        <h5 class="drilldown-alert-title">Data Quality Alerts</h5>
    `;
    qualityWarnings.forEach(function(warning) {
      html += `<p class="drilldown-alert-text">` + warning + `</p>`;
    });
    html += `
      </div>
    `;
  }

  // Create consistent column-to-color mapping for vocabulary tables
  var columnColorMap = {};
  var allColumns = [];
  var colors = ["#ffffff", "#f8fafc"];

  // Collect all unique column names from both source and target vocabularies
  if (tableData.source_vocabularies) {
    tableData.source_vocabularies.forEach(function(v) {
      if (allColumns.indexOf(v.column_name) === -1) {
        allColumns.push(v.column_name);
      }
    });
  }
  if (tableData.target_vocabularies) {
    tableData.target_vocabularies.forEach(function(v) {
      if (allColumns.indexOf(v.column_name) === -1) {
        allColumns.push(v.column_name);
      }
    });
  }

  // Sort columns alphabetically and assign alternating colors
  allColumns.sort();
  allColumns.forEach(function(col, index) {
    columnColorMap[col] = colors[index % 2];
  });

  // Overview Metrics
  var dqdClass = tableData.final_rows === 0 ? "neutral" : getDQDClass(tableData.dqd_score);
  var dqdDisplay = tableData.final_rows === 0 ? "N/A" : (tableData.dqd_score !== null && tableData.dqd_score !== undefined ? tableData.dqd_score + "%" : "N/A");

  html += `
    <div class="metrics-grid">
      <div class="metric-card">
        <div class="metric-label">DQD Score</div>
        <div class="text-center mt-20">
          <div class="dqd-score drilldown-dqd-score ` + dqdClass + `">
            ` + dqdDisplay + `
          </div>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Initial Rows</div>
        <div class="metric-value">` + formatNumber(tableData.initial_rows) + `</div>
        <div class="metric-sublabel">Total before processing</div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Final Rows</div>
        <div class="metric-value">` + formatNumber(tableData.final_rows) + `</div>
        <div class="metric-sublabel">After all processing</div>
      </div>
    </div>
  `;

  // Data Quality Control Section - Always show, highlight issues
  html += `<div class="subsection"><h4>Data Quality Control</h4>`;

  // Cards container for 5 cards in 3-2 layout
  // Top row: 3 cards (Rows Without Connect ID, Referential Integrity, Invalid Rows)
  // Bottom row: 2 cards (Invalid Concepts, Default Dates)
  html += `<div class="quality-cards-grid">`;

  // Row 1: Rows Without Connect ID | Referential Integrity Violations | Invalid Rows
  // Rows Without Connect ID Card
  var missingClass = tableData.final_rows === 0 ? "neutral" : (tableData.missing_person_id_rows > 0 ? "warning" : "success");
  var missingPercent = (tableData.missing_person_id_percent || 0).toFixed(1);
  var missingDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(tableData.missing_person_id_rows) + ` <span class="percentage-display">(` + missingPercent + `%)</span>`);
  html += `
    <div class="metric-card ` + missingClass + `">
      <div class="metric-label">Rows Without Connect ID</div>
      <div class="metric-value">` + missingDisplay + `</div>
      <div class="metric-sublabel">Missing person_id, removed from delivery</div>
    </div>
  `;

  // Referential Integrity Violations Card
  var refIntegrityViolations = tableData.referential_integrity_violations || 0;
  var refIntegrityClass = tableData.final_rows === 0 ? "neutral" : (refIntegrityViolations > 0 ? "warning" : "success");
  var refIntegrityPercent = (tableData.referential_integrity_percent || 0).toFixed(1);
  var refIntegrityDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(refIntegrityViolations) + ` <span class="percentage-display">(` + refIntegrityPercent + `%)</span>`);
  html += `
    <div class="metric-card ` + refIntegrityClass + `">
      <div class="metric-label">Referential Integrity Violations</div>
      <div class="metric-value">` + refIntegrityDisplay + `</div>
      <div class="metric-sublabel">Rows with person_id not in person table</div>
    </div>
  `;

  // Invalid Rows Card
  var invalidClass = tableData.final_rows === 0 ? "neutral" : (tableData.invalid_rows > 0 ? "warning" : "success");
  var invalidPercent = (tableData.invalid_rows_percent || 0).toFixed(1);
  var invalidDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(tableData.invalid_rows) + ` <span class="percentage-display">(` + invalidPercent + `%)</span>`);
  html += `
    <div class="metric-card ` + invalidClass + `">
      <div class="metric-label">Invalid Rows</div>
      <div class="metric-value">` + invalidDisplay + `</div>
      <div class="metric-sublabel">Did not meet OMOP data type specs</div>
    </div>
  `;

  // Row 2: Rows with Invalid Concepts | Rows with Default Dates
  // Invalid Concepts Card (variables already declared at top for warnings)
  var invalidConceptClass = tableData.final_rows === 0 ? "neutral" : (invalidConceptRows > 0 ? "warning" : "success");
  var invalidConceptDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(invalidConceptRows) + ` <span class="percentage-display">(` + invalidConceptPercent + `%)</span>`);
  html += `
    <div class="metric-card ` + invalidConceptClass + `">
      <div class="metric-label">Rows with Invalid Concepts</div>
      <div class="metric-value">` + invalidConceptDisplay + `</div>
      <div class="metric-sublabel">Concept IDs not in vocabulary tables</div>
    </div>
  `;

  // Default Date Values Card (variables already declared at top for warnings)
  // Determine color based on percentage: 0-1% green, >1-15% yellow, >15% red
  var defaultDateClass = "success";
  if (tableData.final_rows === 0) {
    defaultDateClass = "neutral";
  } else if (parseFloat(defaultDatePercent) > 15) {
    defaultDateClass = "warning";  // Use warning for >15% (will show red-ish)
  } else if (parseFloat(defaultDatePercent) > 1) {
    defaultDateClass = "warning";
  }

  var defaultDateDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(defaultDateRows) + ` <span class="percentage-display">(` + defaultDatePercent + `%)</span>`);

  html += `
    <div class="metric-card ` + defaultDateClass + `">
      <div class="metric-label">Rows with Default Dates</div>
      <div class="metric-value">` + defaultDateDisplay + `</div>
      <div class="metric-sublabel">Placeholder date values (e.g., 1900-01-01)</div>
    </div>
  `;

  html += `</div>`;  // End cards grid

  // Extra Columns Removed - Always show for delivered tables
  if (tableData.delivered) {
    html += `
      <div class="drilldown-section-spacing">
        <h5 style="margin: 0 0 10px 0;">Extra Columns Removed</h5>
        <div class="info-box">
    `;

    if (tableData.invalid_columns && tableData.invalid_columns.length > 0) {
      var columnCount = tableData.invalid_columns.length;
      var columnWord = columnCount === 1 ? "column was" : "columns were";
      html += `
          <p><strong>` + columnCount + `</strong> non-OMOP ` + columnWord + ` removed:</p>
          <p style="font-family: monospace; color: #666;">` + tableData.invalid_columns.join(", ") + `</p>
      `;
    } else {
      html += `
          <p>No extra columns were removed.</p>
      `;
    }

    html += `
        </div>
      </div>
    `;
  }

  // Missing Columns Added - Always show for delivered tables
  if (tableData.delivered) {
    html += `
      <div class="drilldown-section-spacing">
        <h5 style="margin: 0 0 10px 0;">Missing Columns Added</h5>
        <div class="info-box">
    `;

    if (tableData.missing_columns && (typeof tableData.missing_columns === "string" || tableData.missing_columns.length > 0)) {
      // Handle both string (single column) and array (multiple columns) cases
      var missingColumnsArray = typeof tableData.missing_columns === "string" ? [tableData.missing_columns] : tableData.missing_columns;
      var missingColumnCount = missingColumnsArray.length;
      var missingColumnWord = missingColumnCount === 1 ? "column was" : "columns were";
      html += `
          <p><strong>` + missingColumnCount + `</strong> OMOP ` + missingColumnWord + ` added:</p>
          <p style="font-family: monospace; color: #666;">` + missingColumnsArray.join(", ") + `</p>
      `;
    } else {
      html += `
          <p>All required OMOP columns were present.</p>
      `;
    }

    html += `
        </div>
      </div>
    `;
  }

  html += `</div>`;  // End Data Quality Control subsection

  // Type Concepts
  if (tableData.type_concepts && tableData.type_concepts.length > 0) {
    html += `
      <div class="subsection">
        <h4 style="margin-bottom: 4px;">Type Concept Breakdown</h4>
        <div style="font-size: 0.9em; color: #94a3b8; margin-bottom: 8px; text-align: left;">${tableData.name}</div>
        <div class="chart-container" style="margin-top: 16px;">
          ${buildTypeConceptChart(tableData.type_concepts)}
        </div>
      </div>
    `;
  }

  // Data Timeline Section
  html += `
    <div class="subsection">
      <h4 style="margin-bottom: 4px;">Data Timeline</h4>
      <div style="font-size: 0.9em; color: #94a3b8; margin-bottom: 8px; text-align: center;">${tableData.name}</div>

      <div style="margin-top: 16px;">
        <div class="toggle-buttons drilldown-time-series-controls">
          <button class="toggle-button active" id="drilldown-time-series-recent" data-action="switch-drilldown-time-series-view" data-view="recent">Last 15 Years</button>
          <button class="toggle-button" id="drilldown-time-series-custom" data-action="switch-drilldown-time-series-view" data-view="custom">Custom</button>
        </div>

        <div id="drilldown-time-series-custom-controls" class="drilldown-time-series-custom-controls" style="display: none;">
          <label class="drilldown-time-series-label">From:</label>
          <input type="number" id="drilldown-time-series-start-year" min="1900" max="2100" placeholder="YYYY" class="drilldown-time-series-input">
          <label class="drilldown-time-series-label">To:</label>
          <input type="number" id="drilldown-time-series-end-year" min="1900" max="2100" placeholder="YYYY" class="drilldown-time-series-input">
          <button data-action="apply-drilldown-year-range" class="drilldown-time-series-button">Apply</button>
        </div>

        <div id="table-drilldown-time-series-chart-container" class="drilldown-time-series-chart">
          <!-- Chart will be populated by JavaScript -->
        </div>
      </div>
    </div>
  `;

  // Use pre-computed harmonization data from R
  var rowsIn = tableData.transitions_in || 0;
  var rowsOut = tableData.rows_out || 0;
  var harmonizationNet = tableData.harmonization || 0;

  // Calculate rows added from 1:N same-table mappings
  // Use valid_rows (not initial_rows) as the baseline and add back rows_out
  // This gives us the net expansion from 1:N mappings, separate from rows moved out
  var same_table_result_rows = tableData.same_table_result_rows || 0;
  var valid_rows = tableData.valid_rows || 0;
  var rowsAddedFromMappings = (same_table_result_rows - valid_rows) + rowsOut;

  // Only show harmonization flow if there was actual harmonization activity
  if (harmonizationNet !== 0) {
    // Format rows out (red with minus sign if > 0)
    var rowsOutFormatted = rowsOut > 0 ? '<span style="color: #ef4444;">-' + formatNumber(rowsOut) + '</span>' : formatNumber(rowsOut);

    // Format rows in (green with plus sign if > 0)
    var rowsInFormatted = rowsIn > 0 ? '<span style="color: #10b981;">+' + formatNumber(rowsIn) + '</span>' : formatNumber(rowsIn);

    // Format rows added from mappings (green with plus sign if > 0)
    var rowsAddedFormatted = rowsAddedFromMappings > 0 ? '<span style="color: #10b981;">+' + formatNumber(rowsAddedFromMappings) + '</span>' : formatNumber(rowsAddedFromMappings);

    html += `
      <div class="subsection">
        <h4>Vocabulary Harmonization Flow</h4>
        <div class="info-box">
          <ul style="margin: 0; padding-left: 20px;">
            <li>Rows moved to other tables: ` + rowsOutFormatted + `</li>
            <li>Rows received from other tables: ` + rowsInFormatted + `</li>
            <li>Rows added from 1:N mappings: ` + rowsAddedFormatted + `</li>
          </ul>
    `;

    var netClass = harmonizationNet > 0 ? "harmonization-positive" : (harmonizationNet < 0 ? "harmonization-negative" : "harmonization-neutral");
    var netSign = harmonizationNet > 0 ? "+" : "";

    html += `
          <p style="margin-top: 15px; margin-bottom: 0;"><strong>Net Impact:</strong> <span class="` + netClass + `">` + netSign + formatNumber(harmonizationNet) + ` rows</span></p>
        </div>
      </div>
    `;
  }


  // Transitions and Harmonization
  if (tableData.transitions && tableData.transitions.length > 0) {
    html += `
      <div class="subsection">
        <h4>Vocabulary Harmonization & Table Transitions</h4>
    `;

    // Harmonization Strategies (if available)
    if (tableData.harmonization_statuses && tableData.harmonization_statuses.length > 0) {
      const totalStatusRows = tableData.harmonization_statuses.reduce(function(sum, s) { return sum + s.count; }, 0);

      html += `
        <div class="info-box" style="margin-bottom: 20px;">
          <h5 style="margin-top: 0; margin-bottom: 15px;">Harmonization Strategies</h5>
          <p style="margin-bottom: 15px;">Breakdown of vocabulary harmonization approaches applied to <strong>${tableData.name}</strong>:</p>
          <div style="display: grid; grid-template-columns: 3fr 1fr; gap: 10px; align-items: center; margin-bottom: 10px;">
            <div style="font-weight: 600;">Strategy</div>
            <div style="font-weight: 600; text-align: right;">Count</div>
      `;

      tableData.harmonization_statuses.forEach(function(status) {
        html += `
            <div style="padding: 8px 0; border-top: 1px solid #e5e7eb;">${status.status}</div>
            <div style="padding: 8px 0; border-top: 1px solid #e5e7eb; text-align: right;">${formatNumber(status.count)}</div>
        `;
      });

      // Calculate harmonization impact as percentage of table's initial rows
      const tableInitialRows = tableData.initial_rows || 0;
      const tableHarmonizationPercent = tableInitialRows > 0 ? Math.round((totalStatusRows / tableInitialRows) * 100) : 0;

      html += `
          </div>
          <p style="margin-top: 15px; margin-bottom: 0;"><strong>Total Records Harmonized:</strong> ${formatNumber(totalStatusRows)} out of ${formatNumber(tableInitialRows)} (${tableHarmonizationPercent}% of table's initial rows)</p>
        </div>
      `;
    }

    // Sankey Diagram
    html += `
        <h5 style="margin-top: 30px; margin-bottom: 16px;">Table Transition Flow</h5>
        ${buildSankeyDiagram(tableData.transitions)}
      </div>
    `;
  }

  // Vocabulary Breakdown - Side-by-Side Source and Target Sections (moved to bottom)
  if ((tableData.source_vocabularies && tableData.source_vocabularies.length > 0) ||
      (tableData.target_vocabularies && tableData.target_vocabularies.length > 0)) {
    html += `<div class="subsection"><h4>Vocabulary Breakdown</h4>`;

    html += `<div style="display: grid; grid-template-columns: 1fr 1fr; gap: 40px; margin-top: 24px;">`;

    // Source Vocabularies Section
    if (tableData.source_vocabularies && tableData.source_vocabularies.length > 0) {
      // Sort by column_name first, then by count DESC within each column
      var sortedSourceVocab = tableData.source_vocabularies.sort(function(a, b) {
        if (a.column_name < b.column_name) return -1;
        if (a.column_name > b.column_name) return 1;
        return b.count - a.count;
      });

      // Build rows using consistent column color mapping
      var sourceVocabRows = "";
      sortedSourceVocab.forEach(function(v) {
        var bgColor = columnColorMap[v.column_name] || "#ffffff";
        sourceVocabRows += `
                  <tr style="background-color: ${bgColor};">
                    <td>${v.column_name}</td>
                    <td><strong>${v.vocabulary}</strong></td>
                    <td style="text-align: right;">${formatNumber(v.count)}</td>
                  </tr>`;
      });

      html += `
          <div>
            <h5 style="margin-bottom: 16px; font-size: 1em; color: #475569;">Source Vocabularies</h5>
            <div class="table-container" style="border: 2px solid #e2e8f0; border-radius: 8px;">
              <table class="vocab-table">
                <thead style="background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%);">
                  <tr>
                    <th>Column</th>
                    <th>Vocabulary</th>
                    <th style="text-align: right;">Count</th>
                  </tr>
                </thead>
                <tbody>${sourceVocabRows}
                </tbody>
              </table>
            </div>
          </div>
      `;
    } else {
      html += `<div></div>`; // Empty div for grid spacing
    }

    // Target Vocabularies Section
    if (tableData.target_vocabularies && tableData.target_vocabularies.length > 0) {
      // Sort by column_name first, then by count DESC within each column
      var sortedTargetVocab = tableData.target_vocabularies.sort(function(a, b) {
        if (a.column_name < b.column_name) return -1;
        if (a.column_name > b.column_name) return 1;
        return b.count - a.count;
      });

      // Build rows using consistent column color mapping
      var targetVocabRows = "";
      sortedTargetVocab.forEach(function(v) {
        var bgColor = columnColorMap[v.column_name] || "#ffffff";
        targetVocabRows += `
                  <tr style="background-color: ${bgColor};">
                    <td>${v.column_name}</td>
                    <td><strong>${v.vocabulary}</strong></td>
                    <td style="text-align: right;">${formatNumber(v.count)}</td>
                  </tr>`;
      });

      html += `
          <div>
            <h5 style="margin-bottom: 16px; font-size: 1em; color: #475569;">Target Vocabularies</h5>
            <div class="table-container" style="border: 2px solid #e2e8f0; border-radius: 8px;">
              <table class="vocab-table">
                <thead style="background: linear-gradient(135deg, #f1f5f9 0%, #e2e8f0 100%);">
                  <tr>
                    <th>Column</th>
                    <th>Vocabulary</th>
                    <th style="text-align: right;">Count</th>
                  </tr>
                </thead>
                <tbody>${targetVocabRows}
                </tbody>
              </table>
            </div>
          </div>
      `;
    } else {
      html += `<div></div>`; // Empty div for grid spacing
    }

    html += `</div></div>`; // Close grid and subsection
  }

  return html;
}

// ============================================================================
// GROUP TYPE CONCEPT UPDATES
// ============================================================================

function updateGroupTypeConcepts(groupName) {
  console.log("==== updateGroupTypeConcepts ====");
  console.log("Group name:", groupName);

  const groupData = getGroupData(groupName);
  console.log("Group data:", groupData);
  console.log("Has type_concepts:", groupData && groupData.type_concepts);
  console.log("Type concepts length:", groupData && groupData.type_concepts && groupData.type_concepts.length);

  if (!groupData || !groupData.type_concepts || groupData.type_concepts.length === 0) {
    const groupId = groupName.toLowerCase().replace(/ /g, "-");
    console.log("No type concepts, looking for container:", "group-type-concepts-" + groupId);
    const container = document.getElementById("group-type-concepts-" + groupId);
    console.log("Container found:", container !== null);
    if (container) {
      container.innerHTML = "<p>No type concept data available for this group</p>";
    }
    return;
  }

  const groupId = groupName.toLowerCase().replace(/ /g, "-");
  console.log("Looking for container:", "group-type-concepts-" + groupId);
  const container = document.getElementById("group-type-concepts-" + groupId);
  console.log("Container found:", container !== null);

  if (container) {
    const html = buildTypeConceptChart(groupData.type_concepts);
    container.innerHTML = html;
    console.log("Type concepts updated successfully");
  } else {
    console.warn("Container not found for group ID:", groupId);
  }
}

// ============================================================================
// VOCABULARY HARMONIZATION
// ============================================================================

function initializeVocabHarmonization() {
  console.log("==== initializeVocabHarmonization ====");

  const container = document.getElementById("vocab-harm-content");
  if (!container) {
    console.warn("Vocabulary harmonization container not found");
    return;
  }

  const transitions = REPORT_DATA.overall_transitions;
  console.log("Overall transitions:", transitions);
  console.log("Transitions length:", transitions ? transitions.length : 0);

  if (!transitions || transitions.length === 0) {
    container.innerHTML = "<p>No vocabulary harmonization data available</p>";
    return;
  }

  const html = buildVocabHarmonizationContent(transitions);
  container.innerHTML = html;
  console.log("Vocabulary harmonization initialized successfully");
}

function buildVocabHarmonizationContent(transitions) {
  let html = "";

  // Build harmonization strategies breakdown
  const harmonizationStatuses = REPORT_DATA.harmonization_statuses;
  if (harmonizationStatuses && harmonizationStatuses.length > 0) {
    const totalStatusRows = harmonizationStatuses.reduce(function(sum, s) { return sum + s.count; }, 0);

    html += "<h5 style=\"margin-top: 0;\">Harmonization Strategies</h5>";
    html += "<div class=\"info-box\" style=\"margin-bottom: 20px;\">";
    html += "<p style=\"margin-bottom: 15px;\">Breakdown of vocabulary harmonization approaches applied during processing:</p>";
    html += "<div style=\"display: grid; grid-template-columns: 3fr 1fr; gap: 10px; align-items: center; margin-bottom: 10px;\">";
    html += "<div style=\"font-weight: 600;\">Strategy</div>";
    html += "<div style=\"font-weight: 600; text-align: right;\">Count</div>";

    harmonizationStatuses.forEach(function(status) {
      html += "<div style=\"padding: 8px 0; border-top: 1px solid #e5e7eb;\">" + status.status + "</div>";
      html += "<div style=\"padding: 8px 0; border-top: 1px solid #e5e7eb; text-align: right;\">" + formatNumber(status.count) + "</div>";
    });

    html += "</div>";

    // Calculate harmonization impact as percentage of original dataset
    const totalInitial = REPORT_DATA.total_initial_rows || 0;
    const harmonizationPercent = totalInitial > 0 ? Math.round((totalStatusRows / totalInitial) * 100) : 0;

    html += "<p style=\"margin-top: 15px; margin-bottom: 0;\"><strong>Total Records Harmonized:</strong> " +
            formatNumber(totalStatusRows) + " out of " + formatNumber(totalInitial) +
            " (" + harmonizationPercent + "% of original dataset)</p>";
    html += "</div>";
  }

  // Use pre-computed transition statistics from R
  const transitionStats = REPORT_DATA.overall_transition_stats;
  const totalRows = transitionStats.total_rows || 0;
  const sameTableCount = transitionStats.same_table_count || 0;
  const crossTableCount = transitionStats.cross_table_count || 0;

  // Calculate percentages for display
  const sameTablePercent = totalRows > 0 ? Math.round((sameTableCount / totalRows) * 100) : 0;
  const crossTablePercent = totalRows > 0 ? Math.round((crossTableCount / totalRows) * 100) : 0;

  // Build Table Transition Flow section
  html += "<h5 style=\"margin-top: 30px;\">Table Transition Flow</h5>";
  html += "<div class=\"info-box\" style=\"margin-bottom: 20px;\">";
  html += "<p><strong>Total Rows Processed:</strong> " + formatNumber(totalRows) + "</p>";
  html += "<p><strong>Rows Staying in Same Table:</strong> " + formatNumber(sameTableCount) + " (" + sameTablePercent + "%)</p>";
  html += "<p style=\"margin-bottom: 0;\"><strong>Rows Moving Between Tables:</strong> " + formatNumber(crossTableCount) + " (" + crossTablePercent + "%)</p>";
  html += "</div>";

  html += buildSankeyDiagram(transitions);

  return html;
}

function buildSankeyDiagram(transitions) {
  if (!transitions || transitions.length === 0) {
    return "<p>No transitions to display</p>";
  }

  // Use table colors from configuration
  const tableColors = REPORT_DATA.table_colors || {};

  // Get unique source and target tables
  const sourceSet = new Set();
  const targetSet = new Set();
  transitions.forEach(function(t) {
    sourceSet.add(t.source_table);
    targetSet.add(t.target_table);
  });

  const sources = Array.from(sourceSet).sort();
  const targets = Array.from(targetSet).sort();

  // Calculate node values based on total flow
  const sourceFlows = {};
  const targetFlows = {};

  sources.forEach(function(s) { sourceFlows[s] = 0; });
  targets.forEach(function(t) { targetFlows[t] = 0; });

  transitions.forEach(function(t) {
    sourceFlows[t.source_table] += t.count;
    targetFlows[t.target_table] += t.count;
  });

  // Find max flow for scaling
  const maxFlow = Math.max(
    Math.max.apply(Math, Object.values(sourceFlows)),
    Math.max.apply(Math, Object.values(targetFlows))
  );

  // SVG dimensions and layout
  const width = 900;
  const nodeWidth = 20;
  const minNodeSpacing = 35; // Minimum space between nodes
  const maxNodeHeight = 50;
  const minNodeHeight = 12;
  const leftX = 180;
  const rightX = width - 180;

  // Calculate node positions and heights first to determine actual height needed
  const sourceNodes = {};
  const targetNodes = {};

  let sourceY = 50;
  sources.forEach(function(source) {
    const flowRatio = sourceFlows[source] / maxFlow;
    const nodeHeight = minNodeHeight + (flowRatio * (maxNodeHeight - minNodeHeight));
    sourceNodes[source] = {
      x: leftX,
      y: sourceY,
      height: nodeHeight,
      value: sourceFlows[source]
    };
    sourceY += nodeHeight + minNodeSpacing; // Use node height + spacing
  });

  let targetY = 50;
  targets.forEach(function(target) {
    const flowRatio = targetFlows[target] / maxFlow;
    const nodeHeight = minNodeHeight + (flowRatio * (maxNodeHeight - minNodeHeight));
    targetNodes[target] = {
      x: rightX,
      y: targetY,
      height: nodeHeight,
      value: targetFlows[target]
    };
    targetY += nodeHeight + minNodeSpacing; // Use node height + spacing
  });

  // Calculate actual height needed based on the tallest column
  const height = Math.max(sourceY, targetY) + 10;

  // Sort transitions by count for rendering
  const sortedTransitions = transitions.slice().sort(function(a, b) { return b.count - a.count; });

  // Build SVG
  let html = "<div class=\"sankey-diagram\" style=\"padding: 15px 20px; background: #f8fafc; border: 1px solid #e2e8f0; overflow-x: auto;\">";
  html += "<svg width=\"" + width + "\" height=\"" + height + "\" style=\"font-family: system-ui, -apple-system, sans-serif; display: block;\">";

  // Track vertical offsets for stacking flows from each node
  const sourceOffsets = {};
  const targetOffsets = {};
  sources.forEach(function(s) { sourceOffsets[s] = 0; });
  targets.forEach(function(t) { targetOffsets[t] = 0; });

  // Draw flows (links) - draw them first so they appear behind nodes
  sortedTransitions.forEach(function(transition) {
    const source = transition.source_table;
    const target = transition.target_table;
    const value = transition.count;

    const sourceNode = sourceNodes[source];
    const targetNode = targetNodes[target];

    if (!sourceNode || !targetNode) return;

    // Calculate link height proportional to value
    const flowRatio = value / sourceFlows[source];
    const linkHeight = flowRatio * sourceNode.height;

    // Calculate start and end positions
    const x1 = sourceNode.x + nodeWidth;
    const y1 = sourceNode.y + sourceOffsets[source] + (linkHeight / 2);
    const x2 = targetNode.x;
    const y2 = targetNode.y + targetOffsets[target] + (linkHeight / 2);

    // Update offsets for next link
    sourceOffsets[source] += linkHeight;
    targetOffsets[target] += (value / targetFlows[target]) * targetNode.height;

    // Create curved path using cubic Bezier
    const controlPointX = (x1 + x2) / 2;
    const path = "M" + x1 + "," + y1 +
                 " C" + controlPointX + "," + y1 +
                 " " + controlPointX + "," + y2 +
                 " " + x2 + "," + y2;

    // Use source table color for the flow
    const color = tableColors[source] || "#64748b";
    const opacity = 0.5;

    html += "<path d=\"" + path + "\" stroke=\"" + color + "\" stroke-width=\"" + Math.max(1, linkHeight) + "\" fill=\"none\" opacity=\"" + opacity + "\" style=\"cursor: pointer;\">";
    html += "<title>" + source + " → " + target + ": " + formatNumber(value) + " records</title>";
    html += "</path>";
  });

  // Draw source nodes
  sources.forEach(function(source) {
    const node = sourceNodes[source];
    const nodeColor = tableColors[source] || "#0f172a";
    html += "<rect x=\"" + node.x + "\" y=\"" + node.y + "\" width=\"" + nodeWidth + "\" height=\"" + node.height + "\" fill=\"" + nodeColor + "\" rx=\"2\"/>";
    html += "<text x=\"" + (node.x - 10) + "\" y=\"" + (node.y + node.height / 2) + "\" text-anchor=\"end\" dominant-baseline=\"middle\" font-size=\"13\" font-weight=\"500\" fill=\"#1e293b\">" + source + "</text>";
    html += "<text x=\"" + (node.x - 10) + "\" y=\"" + (node.y + node.height / 2 + 14) + "\" text-anchor=\"end\" dominant-baseline=\"middle\" font-size=\"11\" fill=\"#64748b\">" + formatNumber(node.value) + "</text>";
  });

  // Draw target nodes
  targets.forEach(function(target) {
    const node = targetNodes[target];
    const nodeColor = tableColors[target] || "#0f172a";
    html += "<rect x=\"" + node.x + "\" y=\"" + node.y + "\" width=\"" + nodeWidth + "\" height=\"" + node.height + "\" fill=\"" + nodeColor + "\" rx=\"2\"/>";
    html += "<text x=\"" + (node.x + nodeWidth + 10) + "\" y=\"" + (node.y + node.height / 2) + "\" text-anchor=\"start\" dominant-baseline=\"middle\" font-size=\"13\" font-weight=\"500\" fill=\"#1e293b\">" + target + "</text>";
    html += "<text x=\"" + (node.x + nodeWidth + 10) + "\" y=\"" + (node.y + node.height / 2 + 14) + "\" text-anchor=\"start\" dominant-baseline=\"middle\" font-size=\"11\" fill=\"#64748b\">" + formatNumber(node.value) + "</text>";
  });

  // Add column headers
  html += "<text x=\"" + (leftX + nodeWidth / 2) + "\" y=\"30\" text-anchor=\"middle\" font-size=\"14\" font-weight=\"600\" fill=\"#0f172a\">Source</text>";
  html += "<text x=\"" + (rightX + nodeWidth / 2) + "\" y=\"30\" text-anchor=\"middle\" font-size=\"14\" font-weight=\"600\" fill=\"#0f172a\">Target</text>";

  html += "</svg>";
  html += "</div>";

  return html;
}

// ============================================================================
// CHART BUILDERS
// ============================================================================

function buildTypeConceptChartSimple(typeConceptsData) {
  if (!typeConceptsData || typeConceptsData.length === 0) {
    return "<p>No type concept data available</p>";
  }

  // Group by type_group and aggregate
  var grouped = {};
  typeConceptsData.forEach(function(tc) {
    var group = tc.type_group;
    if (!grouped[group]) {
      grouped[group] = { group: group, count: 0, concepts: [] };
    }
    grouped[group].count += tc.count;
    grouped[group].concepts.push(tc);
  });

  // Define canonical order
  var typeGroupOrder = REPORT_DATA.type_group_order || [
    "EHR", "Claims", "Disease registry", "Patient reported", "Other", "Unlabeled"
  ];

  // Ensure all groups from canonical order are present (with 0 counts if missing)
  typeGroupOrder.forEach(function(groupName) {
    if (!grouped[groupName]) {
      grouped[groupName] = { group: groupName, count: 0, concepts: [] };
    }
  });

  // Convert to array and sort by canonical order (not by count)
  var groupArray = [];
  for (var key in grouped) {
    if (grouped.hasOwnProperty(key)) {
      groupArray.push(grouped[key]);
    }
  }

  // Sort by canonical order
  var sortedGroups = groupArray.sort(function(a, b) {
    var indexA = typeGroupOrder.indexOf(a.group);
    var indexB = typeGroupOrder.indexOf(b.group);
    if (indexA === -1) indexA = 999; // Put unknown groups at end
    if (indexB === -1) indexB = 999;
    return indexA - indexB;
  });

  // Calculate total for percentages
  var total = 0;
  sortedGroups.forEach(function(g) { total += g.count; });

  var html = "";

  html += '<div style="display: flex; gap: 48px; margin: 20px 0; align-items: flex-start; justify-content: center;">';

  html += '<div style="flex-shrink: 0; padding-top: 40px;">';
  html += buildPieChart(sortedGroups, total);
  html += '</div>';

  html += '<div style="display: flex; flex-direction: column; gap: 12px; flex-shrink: 0;">';
  html += '<h5 style="margin: 0 0 8px 0; font-size: 0.85em; color: #64748b; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;">Summary</h5>';
  sortedGroups.forEach(function(group) {
    var color = REPORT_DATA.type_colors[group.group] || "#7AA6DC";
    var percentage = ((group.count / total) * 100).toFixed(1);

    html += '<div style="display: flex; align-items: center; gap: 10px;">';
    html += '  <div style="width: 20px; height: 20px; background-color: ' + color + '; border: 1px solid #e5e7eb;"></div>';
    html += '  <div style="min-width: 200px;">';
    html += '    <div style="font-weight: 500; color: #0f172a;">' + group.group + '</div>';
    html += '    <div style="font-size: 0.9em; color: #64748b;">' + formatNumber(group.count) + ' (' + percentage + '%)</div>';
    html += '  </div>';
    html += '</div>';
  });
  html += '</div>';

  html += '</div>';

  return html;
}

function buildTypeConceptChart(typeConceptsData) {
  if (!typeConceptsData || typeConceptsData.length === 0) {
    return "<p>No type concept data available</p>";
  }

  // Group by type_group and aggregate
  var grouped = {};
  typeConceptsData.forEach(function(tc) {
    var group = tc.type_group;
    if (!grouped[group]) {
      grouped[group] = { group: group, count: 0, concepts: [] };
    }
    grouped[group].count += tc.count;
    grouped[group].concepts.push(tc);
  });

  // Define canonical order
  var typeGroupOrder = REPORT_DATA.type_group_order || [
    "EHR", "Claims", "Disease registry", "Patient reported", "Other", "Unlabeled"
  ];

  // Ensure all groups from canonical order are present (with 0 counts if missing)
  typeGroupOrder.forEach(function(groupName) {
    if (!grouped[groupName]) {
      grouped[groupName] = { group: groupName, count: 0, concepts: [] };
    }
  });

  // Convert to array and sort by canonical order (not by count)
  var groupArray = [];
  for (var key in grouped) {
    if (grouped.hasOwnProperty(key)) {
      groupArray.push(grouped[key]);
    }
  }

  // Sort by canonical order
  var sortedGroups = groupArray.sort(function(a, b) {
    var indexA = typeGroupOrder.indexOf(a.group);
    var indexB = typeGroupOrder.indexOf(b.group);
    if (indexA === -1) indexA = 999; // Put unknown groups at end
    if (indexB === -1) indexB = 999;
    return indexA - indexB;
  });

  // Calculate total for percentages
  var total = 0;
  sortedGroups.forEach(function(g) { total += g.count; });

  var html = "";

  html += '<div style="display: flex; gap: 48px; margin: 20px 0; align-items: flex-start; justify-content: center;">';

  html += '<div style="flex-shrink: 0; display: flex; align-items: center; padding-top: 40px;">';
  html += buildPieChart(sortedGroups, total);
  html += '</div>';

  html += '<div style="display: flex; flex-direction: column; gap: 12px; flex-shrink: 0;">';
  html += '<h5 style="margin: 0 0 8px 0; font-size: 0.85em; color: #64748b; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;">Summary</h5>';
  sortedGroups.forEach(function(group) {
    var color = REPORT_DATA.type_colors[group.group] || "#7AA6DC";
    var percentage = ((group.count / total) * 100).toFixed(1);

    html += '<div style="display: flex; align-items: center; gap: 10px;">';
    html += '  <div style="width: 20px; height: 20px; background-color: ' + color + '; border: 1px solid #e5e7eb;"></div>';
    html += '  <div style="min-width: 200px;">';
    html += '    <div style="font-weight: 500; color: #0f172a;">' + group.group + '</div>';
    html += '    <div style="font-size: 0.9em; color: #64748b;">' + formatNumber(group.count) + ' (' + percentage + '%)</div>';
    html += '  </div>';
    html += '</div>';
  });
  html += '</div>';

  html += '<div style="flex: 1; border-left: 1px solid #e5e7eb; padding-left: 48px; min-width: 300px;">';
  html += '<h5 style="margin: 0 0 16px 0; font-size: 0.85em; color: #64748b; font-weight: 600; text-transform: uppercase; letter-spacing: 0.5px;">Detailed Breakdown</h5>';
  sortedGroups.forEach(function(group) {
    // Only show groups with non-zero counts in detailed breakdown
    if (group.count > 0 && group.concepts.length > 0) {
      html += '<div style="margin-bottom: 20px;">';
      html += '  <div style="font-weight: 600; color: #0f172a; margin-bottom: 8px; font-size: 0.9em;">' + group.group + '</div>';
      html += '  <div style="margin-left: 16px; font-size: 0.85em; color: #475569;">';
      group.concepts.forEach(function(concept) {
        html += '<div style="padding: 3px 0;">• ' + concept.type_concept + ': ' + formatNumber(concept.count) + '</div>';
      });
      html += '  </div>';
      html += '</div>';
    }
  });
  html += '</div>';

  html += '</div>';

  return html;
}

function buildPieChart(sortedGroups, total) {
  var size = 300;
  var radius = 120;
  var cx = size / 2;
  var cy = size / 2;

  var html = `<svg width="` + size + `" height="` + size + `" style="display: block;">`;

  var currentAngle = -90;

  sortedGroups.forEach(function(group) {
    var color = REPORT_DATA.type_colors[group.group] || "#7AA6DC";
    var percentage = (group.count / total) * 100;
    var sliceAngle = (group.count / total) * 360;

    // Special case: if this is a full circle (or very close to it), draw a circle instead of arc
    if (sliceAngle >= 359.9) {
      html += `<circle cx="` + cx + `" cy="` + cy + `" r="` + radius + `" fill="` + color + `" stroke="#ffffff" stroke-width="2" style="cursor: pointer;">`;
      html += `  <title>` + group.group + `: ` + formatNumber(group.count) + ` (` + percentage.toFixed(1) + `%)</title>`;
      html += `</circle>`;
    } else {
      var startAngle = currentAngle * (Math.PI / 180);
      var endAngle = (currentAngle + sliceAngle) * (Math.PI / 180);

      var x1 = cx + radius * Math.cos(startAngle);
      var y1 = cy + radius * Math.sin(startAngle);
      var x2 = cx + radius * Math.cos(endAngle);
      var y2 = cy + radius * Math.sin(endAngle);

      var largeArcFlag = sliceAngle > 180 ? 1 : 0;

      var pathData = "M " + cx + " " + cy +
                     " L " + x1 + " " + y1 +
                     " A " + radius + " " + radius + " 0 " + largeArcFlag + " 1 " + x2 + " " + y2 +
                     " Z";

      html += `<path d="` + pathData + `" fill="` + color + `" stroke="#ffffff" stroke-width="2" style="cursor: pointer;">`;
      html += `  <title>` + group.group + `: ` + formatNumber(group.count) + ` (` + percentage.toFixed(1) + `%)</title>`;
      html += `</path>`;
    }

    currentAngle += sliceAngle;
  });

  var innerRadius = 50;
  html += `<circle cx="` + cx + `" cy="` + cy + `" r="` + innerRadius + `" fill="#ffffff" />`;

  html += `</svg>`;

  return html;
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

function formatNumber(num) {
  if (num === null || num === undefined) return "0";
  return num.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function getDQDClass(score) {
  if (score === null || score === undefined) return "";
  if (score >= 95) return "good";
  if (score >= 85) return "fair";
  return "poor";
}

// ============================================================================
// CSV EXPORT
// ============================================================================

function exportTableToCSV() {
  // Get the current group name for the filename
  const selector = document.getElementById("table-group-selector");
  const groupName = selector ? selector.value : "All Tables";

  // Find the visible table in the delivery report section
  const visibleGroup = document.querySelector(".table-group-content:not([style*=\"display: none\"])");
  if (!visibleGroup) {
    alert("No table visible to export");
    return;
  }

  const table = visibleGroup.querySelector("table");
  if (!table) {
    alert("No table found to export");
    return;
  }

  // Extract table data
  const rows = [];

  // Get headers
  const headers = [];
  const headerCells = table.querySelectorAll("thead th");
  headerCells.forEach(function(th) {
    headers.push(th.textContent.trim());
  });
  rows.push(headers);

  // Get data rows
  const dataRows = table.querySelectorAll("tbody tr");
  dataRows.forEach(function(tr) {
    const rowData = [];
    const cells = tr.querySelectorAll("td");
    cells.forEach(function(td) {
      // Clean up the cell text (remove extra whitespace, get text only)
      let text = td.textContent.trim();
      // Escape quotes and wrap in quotes if contains comma
      if (text.includes(",") || text.includes("\"") || text.includes("\n")) {
        text = "\"" + text.replace(/"/g, "\"\"\"") + "\"";
      }
      rowData.push(text);
    });
    rows.push(rowData);
  });

  // Create CSV content
  const csvContent = rows.map(function(row) {
    return row.join(",");
  }).join("\n");

  // Create blob and download
  const blob = new Blob([csvContent], { type: "text/csv;charset=utf-8;" });
  const link = document.createElement("a");
  const url = URL.createObjectURL(blob);

  // Create filename with current date
  const date = new Date().toISOString().split("T")[0];
  const filename = "delivery_report_" + groupName.toLowerCase().replace(/ /g, "_") + "_" + date + ".csv";

  link.setAttribute("href", url);
  link.setAttribute("download", filename);
  link.style.visibility = "hidden";
  document.body.appendChild(link);
  link.click();
  document.body.removeChild(link);
}

// ============================================================================
// SIDEBAR NAVIGATION
// ============================================================================

function scrollToSection(event, sectionId) {
  event.preventDefault();

  var section = document.getElementById(sectionId);
  if (section) {
    section.scrollIntoView({ behavior: "smooth", block: "start" });

    // Update active nav item
    var navItems = document.querySelectorAll(".sidebar-nav-item");
    navItems.forEach(function(item) {
      item.classList.remove("active");
    });
    event.target.classList.add("active");
  }
}

// Track scroll position and update active nav item
function updateActiveNavOnScroll() {
  var sections = document.querySelectorAll(".section");
  var scrollPos = window.scrollY + 200;

  sections.forEach(function(section) {
    var sectionTop = section.offsetTop;
    var sectionHeight = section.offsetHeight;
    var sectionId = section.getAttribute("id");

    if (scrollPos >= sectionTop && scrollPos < sectionTop + sectionHeight) {
      var navItems = document.querySelectorAll(".sidebar-nav-item");
      navItems.forEach(function(item) {
        item.classList.remove("active");
        if (item.getAttribute("href") === "#" + sectionId) {
          item.classList.add("active");
        }
      });
    }
  });
}

// ============================================================================
// INITIALIZATION
// ============================================================================

document.addEventListener("DOMContentLoaded", function() {
  console.log("OMOP Delivery Report initialized");
  console.log("Report data loaded:", REPORT_DATA ? "Yes" : "No");

  // Initialize dataset-wide type concepts
  initializeDatasetTypeConcepts();

  // Set initial table group
  switchTableGroup("Clinical Data");

  // Initialize vocabulary harmonization section
  initializeVocabHarmonization();

  // Initialize data timeline section
  initializeTimeSeries();

  // Set up scroll tracking for sidebar navigation
  window.addEventListener("scroll", updateActiveNavOnScroll);
  updateActiveNavOnScroll();

  // Event delegation for data-action attributes
  document.addEventListener("click", function(event) {
    const target = event.target.closest("[data-action]");
    if (!target) return;

    const action = target.dataset.action;

    switch (action) {
      case "scroll-to-section":
        scrollToSection(event, target.dataset.section);
        break;
      case "export-table":
        exportTableToCSV();
        break;
      case "show-drilldown":
        showTableDrilldown(target.dataset.table);
        break;
      case "hide-drilldown":
        hideTableDrilldown();
        break;
      case "sort-table":
        sortDeliveryTable(target.dataset.group, parseInt(target.dataset.column), target.dataset.sortType);
        break;
      case "switch-time-series-view":
        switchTimeSeriesView(target.dataset.view);
        break;
      case "apply-year-range":
        applyCustomYearRange();
        break;
      case "switch-drilldown-time-series-view":
        switchTableDrilldownTimeSeriesView(target.dataset.view);
        break;
      case "apply-drilldown-year-range":
        applyTableDrilldownCustomYearRange();
        break;
      case "toggle-table-visibility":
        toggleTableVisibility(target.dataset.table);
        break;
    }
  });

  // Event delegation for change events
  document.addEventListener("change", function(event) {
    const target = event.target.closest("[data-action]");
    if (!target) return;

    const action = target.dataset.action;

    switch (action) {
      case "switch-table-group":
        switchTableGroup(target.value);
        break;
    }
  });

  // Log available groups
  if (REPORT_DATA && REPORT_DATA.groups) {
    console.log("Available table groups:", Object.keys(REPORT_DATA.groups));
  }
});

function initializeDatasetTypeConcepts() {
  console.log("==== initializeDatasetTypeConcepts ====");

  var container = document.getElementById("dataset-type-concepts");
  if (!container) {
    console.warn("Dataset type concepts container not found");
    return;
  }

  // Use the overall type concepts directly from REPORT_DATA
  var allTypeConcepts = REPORT_DATA.overall_type_concepts || [];

  console.log("Total type concepts found:", allTypeConcepts.length);

  if (allTypeConcepts.length === 0) {
    container.innerHTML = "<p>No type concept data available</p>";
    return;
  }

  var html = buildTypeConceptChart(allTypeConcepts);
  container.innerHTML = html;
  console.log("Dataset type concepts initialized successfully");
}

// ============================================================================
// BROWSER HISTORY MANAGEMENT
// ============================================================================

// Handle browser back button
window.addEventListener("popstate", function(event) {
  if (event.state && event.state.view === "drilldown") {
    // Going forward to a drilldown view
    showTableDrilldown(event.state.table);
  } else {
    // Going back to main report
    hideTableDrilldown();
  }
});

// Initialize history state on page load
window.addEventListener("load", function() {
  // Set initial state for the main report view
  history.replaceState({ view: "main" }, "", window.location.pathname);
});

// ============================================================================
// DATA TIMELINE VISUALIZATION
// ============================================================================

// Global state for data timeline (time series)
let currentTimeSeriesView = "recent";
let timeSeriesData = [];
let timeSeriesConfig = {};
let visibleTables = new Set(); // Track which tables are visible
let allUniqueTables = []; // Store ALL unique tables once at initialization
let customStartYear = 1970;
let customEndYear = 2025;

// Global state for table drilldown timeline
let currentTableDrilldownView = "recent";
let currentDrilldownTable = null;
let drilldownCustomStartYear = 1970;
let drilldownCustomEndYear = 2025;

// Use table colors from configuration (set when REPORT_DATA loads)
const TIME_SERIES_COLORS = REPORT_DATA.table_colors || {};

function switchTimeSeriesView(view) {
  console.log("Switching data timeline view to:", view);
  currentTimeSeriesView = view;

  // Update button states
  document.getElementById("btn-recent-view").classList.toggle("active", view === "recent");
  document.getElementById("btn-custom-view").classList.toggle("active", view === "custom");

  // Show/hide custom year controls
  const customControls = document.getElementById("custom-year-controls");
  if (customControls) {
    customControls.style.display = view === "custom" ? "block" : "none";
  }

  // Redraw chart
  drawTimeSeriesChart();
}

function toggleTableVisibility(tableName) {
  console.log("Toggling visibility for:", tableName);

  // Don't allow hiding all tables
  if (visibleTables.size === 1 && visibleTables.has(tableName)) {
    console.log("Cannot hide the last visible table");
    return;
  }

  // Toggle visibility
  if (visibleTables.has(tableName)) {
    visibleTables.delete(tableName);
  } else {
    visibleTables.add(tableName);
  }

  console.log("Visible tables:", Array.from(visibleTables));

  // Redraw chart (this will also update the legend)
  drawTimeSeriesChart();
}

function applyCustomYearRange() {
  console.log("Applying custom year range");

  // Get input values
  const startInput = document.getElementById("custom-start-year");
  const endInput = document.getElementById("custom-end-year");

  if (!startInput || !endInput) {
    console.error("Custom year inputs not found");
    return;
  }

  const startYear = parseInt(startInput.value);
  const endYear = parseInt(endInput.value);

  // Validate
  if (isNaN(startYear) || isNaN(endYear)) {
    alert("Please enter valid years");
    return;
  }

  if (startYear >= endYear) {
    alert("Start year must be less than end year");
    return;
  }

  if (startYear < 1900 || endYear > 2100) {
    alert("Please enter years between 1900 and 2100");
    return;
  }

  // Update state
  customStartYear = startYear;
  customEndYear = endYear;

  console.log("Custom range set to:", startYear, "-", endYear);

  // Redraw chart
  drawTimeSeriesChart();
}

function initializeTimeSeries() {
  console.log("Initializing data timeline...");

  // Load data from embedded JSON
  const dataElement = document.getElementById("time-series-data");
  const configElement = document.getElementById("time-series-config");

  if (!dataElement || !configElement) {
    console.log("Data timeline data not found");
    return;
  }

  try {
    timeSeriesData = JSON.parse(dataElement.textContent);
    timeSeriesConfig = JSON.parse(configElement.textContent);
    console.log("Loaded data timeline data:", timeSeriesData.length, "rows");
    console.log("Config:", timeSeriesConfig);

    // Initialize custom year range from config
    customStartYear = timeSeriesConfig.historicalStartYear || 1970;
    customEndYear = timeSeriesConfig.deliveryYear || 2025;

    // Store ALL unique tables from the dataset (THIS IS THE MASTER LIST!)
    allUniqueTables = [...new Set(timeSeriesData.map(d => d.table_name))].sort();
    console.log("All unique tables:", allUniqueTables);

    // Initialize visible tables (all tables visible by default)
    visibleTables = new Set(allUniqueTables);
    console.log("Initialized visible tables:", Array.from(visibleTables));

    // Draw initial chart
    drawTimeSeriesChart();
  } catch (e) {
    console.error("Error parsing data timeline data:", e);
  }
}

function drawTimeSeriesChart() {
  console.log("Drawing data timeline chart for view:", currentTimeSeriesView);

  const container = document.getElementById("time-series-chart-container");
  if (!container) return;

  // ALWAYS update legend first using the MASTER TABLE LIST
  // This ensures ALL tables are always shown in the legend
  updateTimeSeriesLegend(allUniqueTables);

  if (timeSeriesData.length === 0) {
    container.innerHTML = "<p>No time series data available</p>";
    return;
  }

  // Determine year range based on view
  let startYear, endYear;
  if (currentTimeSeriesView === "recent") {
    startYear = timeSeriesConfig.recentStartYear;
    endYear = timeSeriesConfig.recentEndYear;
  } else if (currentTimeSeriesView === "custom") {
    startYear = customStartYear;
    endYear = customEndYear;
  } else {
    startYear = timeSeriesConfig.historicalStartYear;
    endYear = timeSeriesConfig.historicalEndYear;
  }

  // Filter data by year range, visible tables, and group by table
  const tableData = {};
  timeSeriesData.forEach(function(row) {
    // Only include visible tables
    if (!visibleTables.has(row.table_name)) {
      return;
    }

    if (row.year >= startYear && row.year <= endYear) {
      if (!tableData[row.table_name]) {
        tableData[row.table_name] = [];
      }
      tableData[row.table_name].push({ year: row.year, count: row.count });
    }
  });

  // Sort each table's data by year
  Object.keys(tableData).forEach(function(table) {
    tableData[table].sort(function(a, b) { return a.year - b.year; });
  });

  const tables = Object.keys(tableData).sort();
  console.log("Tables with data:", tables);

  if (tables.length === 0) {
    container.innerHTML = "<p>No data available for the selected time range</p>";
    return;
  }

  // Calculate dimensions
  const containerWidth = container.clientWidth;
  const containerHeight = 500;
  const margin = { top: 20, right: 120, bottom: 60, left: 80 };
  const chartWidth = containerWidth - margin.left - margin.right;
  const chartHeight = containerHeight - margin.top - margin.bottom;

  // Calculate scales
  const allCounts = [];
  Object.keys(tableData).forEach(function(table) {
    tableData[table].forEach(function(d) {
      allCounts.push(d.count);
    });
  });

  const maxCount = Math.max.apply(null, allCounts);
  const minCount = 0;

  // Create x-scale (year)
  const xScale = function(year) {
    return margin.left + (year - startYear) / (endYear - startYear) * chartWidth;
  };

  // Create y-scale (count)
  const yScale = function(count) {
    return margin.top + chartHeight - (count - minCount) / (maxCount - minCount) * chartHeight;
  };

  // Build SVG
  let html = '<svg width="' + containerWidth + '" height="' + containerHeight + '" style="background: white; border-radius: 8px; border: 2px solid #e2e8f0;">';

  // Draw grid lines (horizontal)
  const numGridLines = 5;
  for (let i = 0; i <= numGridLines; i++) {
    const y = margin.top + (chartHeight / numGridLines) * i;
    html += '<line x1="' + margin.left + '" y1="' + y + '" x2="' + (margin.left + chartWidth) + '" y2="' + y + '" stroke="#e2e8f0" stroke-width="1"/>';

    // Y-axis labels
    const labelCount = maxCount - (maxCount / numGridLines) * i;
    html += '<text x="' + (margin.left - 10) + '" y="' + y + '" text-anchor="end" dominant-baseline="middle" font-size="12" fill="#64748b">' + formatNumber(Math.round(labelCount)) + '</text>';
  }

  // Draw x-axis ticks and labels
  const yearRange = endYear - startYear;
  const tickInterval = yearRange > 30 ? 5 : (yearRange > 15 ? 2 : 1);

  for (let year = startYear; year <= endYear; year += tickInterval) {
    const x = xScale(year);
    html += '<line x1="' + x + '" y1="' + (margin.top + chartHeight) + '" x2="' + x + '" y2="' + (margin.top + chartHeight + 6) + '" stroke="#64748b" stroke-width="1"/>';
    html += '<text x="' + x + '" y="' + (margin.top + chartHeight + 20) + '" text-anchor="middle" font-size="12" fill="#64748b">' + year + '</text>';
  }

  // Draw axes
  html += '<line x1="' + margin.left + '" y1="' + margin.top + '" x2="' + margin.left + '" y2="' + (margin.top + chartHeight) + '" stroke="#1e293b" stroke-width="2"/>';
  html += '<line x1="' + margin.left + '" y1="' + (margin.top + chartHeight) + '" x2="' + (margin.left + chartWidth) + '" y2="' + (margin.top + chartHeight) + '" stroke="#1e293b" stroke-width="2"/>';

  // Axis labels
  html += '<text x="' + (margin.left + chartWidth / 2) + '" y="' + (containerHeight - 10) + '" text-anchor="middle" font-size="14" font-weight="600" fill="#0f172a">Year</text>';
  html += '<text x="' + (margin.left - 60) + '" y="' + (margin.top + chartHeight / 2) + '" text-anchor="middle" font-size="14" font-weight="600" fill="#0f172a" transform="rotate(-90 ' + (margin.left - 60) + ' ' + (margin.top + chartHeight / 2) + ')">Row Count</text>';

  // Draw lines for each table
  tables.forEach(function(table) {
    const data = tableData[table];
    const color = TIME_SERIES_COLORS[table] || "#64748b";

    if (data.length === 0) return;

    // Build path
    let pathD = "M";
    data.forEach(function(d, i) {
      const x = xScale(d.year);
      const y = yScale(d.count);
      if (i === 0) {
        pathD += x + "," + y;
      } else {
        pathD += " L" + x + "," + y;
      }
    });

    // Draw line
    html += '<path d="' + pathD + '" stroke="' + color + '" stroke-width="2.5" fill="none" style="cursor: pointer;">';
    html += '<title>' + table + '</title>';
    html += '</path>';

    // Draw points
    data.forEach(function(d) {
      const x = xScale(d.year);
      const y = yScale(d.count);
      const tooltipText = table + ' (' + d.year + '): ' + formatNumber(d.count);
      html += '<circle cx="' + x + '" cy="' + y + '" r="4" fill="' + color + '" stroke="white" stroke-width="1.5" style="cursor: pointer;" class="time-series-point" data-tooltip="' + tooltipText + '" onmouseenter="showTimeSeriesTooltip(event, \'' + tooltipText.replace(/'/g, "\\'") + '\')" onmouseleave="hideTimeSeriesTooltip()">';
      html += '<title>' + table + ' (' + d.year + '): ' + formatNumber(d.count) + '</title>';
      html += '</circle>';
    });
  });

  html += '</svg>';

  container.innerHTML = html;
}

function updateTimeSeriesLegend(allTables) {
  const legendContainer = document.getElementById("time-series-legend");
  if (!legendContainer) return;

  let html = "";
  allTables.forEach(function(table) {
    const color = TIME_SERIES_COLORS[table] || "#64748b";
    const isVisible = visibleTables.has(table);
    const inactiveClass = isVisible ? "" : " inactive";

    html += '<div class="legend-item' + inactiveClass + '" data-action="toggle-table-visibility" data-table="' + table + '">';
    html += '  <div style="display: flex; align-items: center; gap: 8px;">';
    html += '    <div class="legend-line" style="width: 30px; height: 3px; background-color: ' + color + ';"></div>';
    html += '    <span class="legend-label" style="font-size: 0.9em; color: #475569; font-weight: 500;">' + table + '</span>';
    html += '  </div>';
    html += '</div>';
  });

  legendContainer.innerHTML = html;
}

// ============================================================================
// TABLE DRILLDOWN TIMELINE
// ============================================================================

function initializeTableDrilldownTimeSeries(tableName) {
  console.log("Initializing table drilldown timeline for:", tableName);
  currentDrilldownTable = tableName;

  // Set default view to recent
  currentTableDrilldownView = "recent";

  // Set default custom year range from config
  if (timeSeriesConfig.historicalStartYear && timeSeriesConfig.deliveryYear) {
    drilldownCustomStartYear = timeSeriesConfig.historicalStartYear;
    drilldownCustomEndYear = timeSeriesConfig.deliveryYear;
  }

  // Draw the chart
  drawTableDrilldownTimeSeries();
}

function switchTableDrilldownTimeSeriesView(view) {
  console.log("Switching table drilldown view to:", view);
  currentTableDrilldownView = view;

  // Update button states
  const recentBtn = document.getElementById("drilldown-time-series-recent");
  const customBtn = document.getElementById("drilldown-time-series-custom");
  const customControls = document.getElementById("drilldown-time-series-custom-controls");

  if (recentBtn && customBtn && customControls) {
    if (view === "recent") {
      recentBtn.classList.add("active");
      customBtn.classList.remove("active");
      customControls.style.display = "none";
    } else if (view === "custom") {
      recentBtn.classList.remove("active");
      customBtn.classList.add("active");
      customControls.style.display = "flex";

      // Populate inputs with current values
      document.getElementById("drilldown-time-series-start-year").value = drilldownCustomStartYear;
      document.getElementById("drilldown-time-series-end-year").value = drilldownCustomEndYear;
    }
  }

  drawTableDrilldownTimeSeries();
}

function applyTableDrilldownCustomYearRange() {
  const startInput = document.getElementById("drilldown-time-series-start-year");
  const endInput = document.getElementById("drilldown-time-series-end-year");

  const startYear = parseInt(startInput.value);
  const endYear = parseInt(endInput.value);

  if (isNaN(startYear) || isNaN(endYear)) {
    alert("Please enter valid years");
    return;
  }

  if (startYear >= endYear) {
    alert("Start year must be before end year");
    return;
  }

  drilldownCustomStartYear = startYear;
  drilldownCustomEndYear = endYear;

  drawTableDrilldownTimeSeries();
}

function drawTableDrilldownTimeSeries() {
  console.log("Drawing table drilldown timeline for:", currentDrilldownTable);

  const container = document.getElementById("table-drilldown-time-series-chart-container");
  if (!container) {
    console.log("Container not found");
    return;
  }

  if (!currentDrilldownTable || timeSeriesData.length === 0) {
    container.innerHTML = "<p>No time series data available</p>";
    return;
  }

  // Determine year range based on view
  let startYear, endYear;
  if (currentTableDrilldownView === "recent") {
    startYear = timeSeriesConfig.recentStartYear;
    endYear = timeSeriesConfig.recentEndYear;
  } else if (currentTableDrilldownView === "custom") {
    startYear = drilldownCustomStartYear;
    endYear = drilldownCustomEndYear;
  } else {
    startYear = timeSeriesConfig.historicalStartYear;
    endYear = timeSeriesConfig.historicalEndYear;
  }

  // Filter data for this table and year range
  const tableData = timeSeriesData.filter(function(row) {
    return row.table_name === currentDrilldownTable &&
           row.year >= startYear &&
           row.year <= endYear;
  });

  if (tableData.length === 0) {
    container.innerHTML = "<p>No data available for the selected time range</p>";
    return;
  }

  // Sort by year
  tableData.sort(function(a, b) { return a.year - b.year; });

  // Calculate dimensions
  const containerWidth = container.clientWidth;
  const containerHeight = 450;
  const margin = { top: 20, right: 40, bottom: 60, left: 80 };
  const chartWidth = containerWidth - margin.left - margin.right;
  const chartHeight = containerHeight - margin.top - margin.bottom;

  // Calculate scales
  const maxCount = Math.max.apply(null, tableData.map(function(d) { return d.count; }));
  const minCount = 0;

  // Create x-scale (year)
  const xScale = function(year) {
    return margin.left + (year - startYear) / (endYear - startYear) * chartWidth;
  };

  // Create y-scale (count)
  const yScale = function(count) {
    return margin.top + chartHeight - ((count - minCount) / (maxCount - minCount)) * chartHeight;
  };

  // Get color for this table
  const color = TIME_SERIES_COLORS[currentDrilldownTable] || "#3b82f6";

  // Build SVG
  let html = '<svg width="' + containerWidth + '" height="' + containerHeight + '" style="background: white; border-radius: 8px;">';

  // Draw grid lines and Y-axis labels
  const numYTicks = 5;
  for (let i = 0; i <= numYTicks; i++) {
    const count = minCount + (maxCount - minCount) * (i / numYTicks);
    const y = yScale(count);

    // Grid line
    html += '<line x1="' + margin.left + '" y1="' + y + '" x2="' + (margin.left + chartWidth) + '" y2="' + y + '" stroke="#e5e7eb" stroke-width="1" />';

    // Y-axis label
    html += '<text x="' + (margin.left - 10) + '" y="' + (y + 4) + '" text-anchor="end" font-size="11" fill="#64748b">' + formatNumber(Math.round(count)) + '</text>';
  }

  // Draw X-axis (years)
  const yearRange = endYear - startYear;
  const yearStep = yearRange <= 15 ? 1 : Math.ceil(yearRange / 15);

  for (let year = startYear; year <= endYear; year += yearStep) {
    const x = xScale(year);
    const y = margin.top + chartHeight;

    // Tick mark
    html += '<line x1="' + x + '" y1="' + y + '" x2="' + x + '" y2="' + (y + 6) + '" stroke="#94a3b8" stroke-width="1" />';

    // Year label
    html += '<text x="' + x + '" y="' + (y + 20) + '" text-anchor="middle" font-size="11" fill="#64748b">' + year + '</text>';
  }

  // Draw line
  let pathData = "M";
  tableData.forEach(function(d, i) {
    const x = xScale(d.year);
    const y = yScale(d.count);
    if (i === 0) {
      pathData += x + "," + y;
    } else {
      pathData += " L" + x + "," + y;
    }
  });

  html += '<path d="' + pathData + '" fill="none" stroke="' + color + '" stroke-width="3" />';

  // Draw points
  tableData.forEach(function(d) {
    const x = xScale(d.year);
    const y = yScale(d.count);
    const tooltipText = currentDrilldownTable + ' (' + d.year + '): ' + formatNumber(d.count);
    html += '<circle cx="' + x + '" cy="' + y + '" r="5" fill="' + color + '" stroke="white" stroke-width="2" style="cursor: pointer;" class="time-series-point" onmouseenter="showTimeSeriesTooltip(event, \'' + tooltipText.replace(/'/g, "\\'") + '\')" onmouseleave="hideTimeSeriesTooltip()">';
    html += '<title>' + currentDrilldownTable + ' (' + d.year + '): ' + formatNumber(d.count) + '</title>';
    html += '</circle>';
  });

  html += '</svg>';

  container.innerHTML = html;
}

// ============================================================================
// CUSTOM TOOLTIP FOR TIME SERIES
// ============================================================================

function showTimeSeriesTooltip(event, text) {
  // Remove any existing tooltip
  hideTimeSeriesTooltip();

  // Create tooltip element
  const tooltip = document.createElement('div');
  tooltip.id = 'time-series-custom-tooltip';
  tooltip.style.position = 'absolute';
  tooltip.style.background = 'rgba(15, 23, 42, 0.95)';
  tooltip.style.color = 'white';
  tooltip.style.padding = '8px 12px';
  tooltip.style.borderRadius = '6px';
  tooltip.style.fontSize = '13px';
  tooltip.style.fontWeight = '500';
  tooltip.style.pointerEvents = 'none';
  tooltip.style.zIndex = '10000';
  tooltip.style.boxShadow = '0 4px 6px rgba(0, 0, 0, 0.1)';
  tooltip.style.whiteSpace = 'nowrap';
  tooltip.textContent = text;

  document.body.appendChild(tooltip);

  // Position tooltip near cursor
  const x = event.pageX;
  const y = event.pageY;
  tooltip.style.left = (x + 10) + 'px';
  tooltip.style.top = (y - 35) + 'px';
}

function hideTimeSeriesTooltip() {
  const tooltip = document.getElementById('time-series-custom-tooltip');
  if (tooltip) {
    tooltip.remove();
  }
}

// ============================================================================
// END OF JAVASCRIPT
// ============================================================================
  