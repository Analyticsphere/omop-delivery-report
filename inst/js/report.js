
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
  const sectionsToHide = ["overview", "dqd-grid", "delivery-report", "vocab-harmonization", "technical-summary"];
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
  const sectionsToShow = ["overview", "dqd-grid", "delivery-report", "vocab-harmonization", "technical-summary"];
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

  // Calculate quality metrics for warnings
  var defaultDateRows = tableData.default_date_rows || 0;
  var defaultDatePercent = tableData.final_rows > 0 ? ((defaultDateRows / tableData.final_rows) * 100).toFixed(1) : 0;
  var invalidConceptRows = tableData.invalid_concept_rows || 0;
  var invalidConceptPercent = tableData.final_rows > 0 ? ((invalidConceptRows / tableData.final_rows) * 100).toFixed(1) : 0;

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
      <div class="info-box" style="margin-bottom: 20px; background-color: #fef3c7; border-left: 4px solid #f59e0b;">
        <h5 style="margin: 0 0 10px 0; color: #92400e; font-size: 1.1em;">Data Quality Alerts</h5>
    `;
    qualityWarnings.forEach(function(warning) {
      html += `<p style="margin: 5px 0; color: #92400e; font-size: 0.9em;">` + warning + `</p>`;
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
          <div class="dqd-score ` + dqdClass + `" style="min-width: 100px; height: 100px; font-size: 2em;">
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
  var missingPercent = tableData.initial_rows > 0 ? ((tableData.missing_person_id_rows / tableData.initial_rows) * 100).toFixed(1) : 0;
  var missingDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(tableData.missing_person_id_rows) + ` <span style="font-size: 0.6em; color: #666;">(` + missingPercent + `%)</span>`);
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
  var refIntegrityPercent = tableData.final_rows > 0 ? ((refIntegrityViolations / tableData.final_rows) * 100).toFixed(1) : 0;
  var refIntegrityDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(refIntegrityViolations) + ` <span style="font-size: 0.6em; color: #666;">(` + refIntegrityPercent + `%)</span>`);
  html += `
    <div class="metric-card ` + refIntegrityClass + `">
      <div class="metric-label">Referential Integrity Violations</div>
      <div class="metric-value">` + refIntegrityDisplay + `</div>
      <div class="metric-sublabel">person_id not found in person table</div>
    </div>
  `;

  // Invalid Rows Card
  var invalidClass = tableData.final_rows === 0 ? "neutral" : (tableData.invalid_rows > 0 ? "warning" : "success");
  var invalidPercent = tableData.initial_rows > 0 ? ((tableData.invalid_rows / tableData.initial_rows) * 100).toFixed(1) : 0;
  var invalidDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(tableData.invalid_rows) + ` <span style="font-size: 0.6em; color: #666;">(` + invalidPercent + `%)</span>`);
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
  var invalidConceptDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(invalidConceptRows) + ` <span style="font-size: 0.6em; color: #666;">(` + invalidConceptPercent + `%)</span>`);
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

  var defaultDateDisplay = tableData.final_rows === 0 ? "N/A" : (formatNumber(defaultDateRows) + ` <span style="font-size: 0.6em; color: #666;">(` + defaultDatePercent + `%)</span>`);

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
      <div style="margin-top: 20px;">
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
      <div style="margin-top: 20px;">
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
        <h4>Type Concept Breakdown</h4>
        <div class="chart-container">
          ${buildTypeConceptChart(tableData.type_concepts)}
        </div>
      </div>
    `;
  }

  // Vocabulary Harmonization Flow Section
  var rowsIn = 0;
  var rowsOut = 0;
  var rowsStayedOnly = 0;
  var rowsStayedAndCopied = 0;
  var sourceRowsMovedOut = 0;
  var hasDispositionData = false;

  // Get disposition data if available (for display purposes)
  if (tableData.dispositions && tableData.dispositions.length > 0) {
    hasDispositionData = true;
    tableData.dispositions.forEach(function(d) {
      if (d.disposition === "moved to other tables") {
        sourceRowsMovedOut = d.count;
      } else if (d.disposition === "stayed only") {
        rowsStayedOnly = d.count;
      } else if (d.disposition === "stayed and copied") {
        rowsStayedAndCopied = d.count;
      }
    });
  }

  // Get rows IN and OUT from transitions
  if (tableData.transitions) {
    tableData.transitions.forEach(function(t) {
      if (t.target_table === tableData.name && t.source_table !== tableData.name) {
        rowsIn += t.count;
      }
      if (t.source_table === tableData.name && t.target_table !== tableData.name) {
        rowsOut += t.count;
      }
    });
  }

  // For harmonization calculation:
  // Use hybrid approach: transition data (for 1:N mappings) minus "stayed and copied"
  // - rowsOut: all result rows that went to other tables (from transitions)
  // - rowsStayedAndCopied: source rows that also stayed in this table
  // - Subtracting them excludes transitions from rows that did not actually decrease this table
  var rowsMovedOut = rowsOut - rowsStayedAndCopied;

  // Use the correct harmonization value from tableData (calculated in R)
  // Then derive rowsAdded to show the breakdown: harmonization = rowsIn + rowsAdded - rowsMovedOut
  var harmonizationNet = tableData.harmonization || 0;
  var rowsAdded = harmonizationNet - rowsIn + rowsMovedOut;

  if (rowsMovedOut > 0 || rowsIn > 0 || rowsAdded > 0) {
    html += `
      <div class="subsection">
        <h4>Vocabulary Harmonization Flow</h4>
        <div class="info-box">
          <p style="margin-bottom: 15px;">Row movement during vocabulary harmonization:</p>
          <ul style="margin: 0; padding-left: 20px;">
    `;

    if (rowsMovedOut > 0) {
      html += `<li><strong>Rows to other tables:</strong> ` + formatNumber(rowsMovedOut) + `</li>`;
    }
    if (rowsIn > 0) {
      html += `<li><strong>Rows from other tables:</strong> ` + formatNumber(rowsIn) + `</li>`;
    }
    if (rowsAdded > 0) {
      html += `<li><strong>Rows added:</strong> ` + formatNumber(rowsAdded) + ` (from 1:N same-table mappings)</li>`;
    }

    var netClass = harmonizationNet > 0 ? "harmonization-positive" : (harmonizationNet < 0 ? "harmonization-negative" : "harmonization-neutral");
    var netSign = harmonizationNet > 0 ? "+" : "";

    html += `
          </ul>
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

      html += `
          </div>
          <p style="margin-top: 15px; margin-bottom: 0;"><strong>Total Records Harmonized:</strong> ${formatNumber(totalStatusRows)}</p>
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
    html += "<p style=\"margin-top: 15px; margin-bottom: 0;\"><strong>Total Records Harmonized:</strong> " + formatNumber(totalStatusRows) + "</p>";
    html += "</div>";
  }

  // Calculate table transition statistics
  const totalRows = transitions.reduce(function(sum, t) { return sum + t.count; }, 0);
  const sameTableTransitions = transitions.filter(function(t) { return t.source_table === t.target_table; });
  const crossTableTransitions = transitions.filter(function(t) { return t.source_table !== t.target_table; });

  const sameTableCount = sameTableTransitions.reduce(function(sum, t) { return sum + t.count; }, 0);
  const crossTableCount = crossTableTransitions.reduce(function(sum, t) { return sum + t.count; }, 0);

  // Build Table Transition Flow section
  html += "<h5 style=\"margin-top: 30px;\">Table Transition Flow</h5>";
  html += "<div class=\"info-box\" style=\"margin-bottom: 20px;\">";
  html += "<p><strong>Total Rows Processed:</strong> " + formatNumber(totalRows) + "</p>";
  html += "<p><strong>Rows Staying in Same Table:</strong> " + formatNumber(sameTableCount) + " (" + Math.round((sameTableCount / totalRows) * 100) + "%)</p>";
  html += "<p style=\"margin-bottom: 0;\"><strong>Rows Moving Between Tables:</strong> " + formatNumber(crossTableCount) + " (" + Math.round((crossTableCount / totalRows) * 100) + "%)</p>";
  html += "</div>";

  html += buildSankeyDiagram(transitions);

  return html;
}

function buildSankeyDiagram(transitions) {
  if (!transitions || transitions.length === 0) {
    return "<p>No transitions to display</p>";
  }

  // Define consistent color palette for clinical tables
  const tableColors = {
    "condition_occurrence": "#ef4444",  // Red - conditions/diseases
    "device_exposure": "#8b5cf6",       // Purple - medical devices
    "drug_exposure": "#3b82f6",         // Blue - medications
    "measurement": "#14b8a6",           // Teal - lab results/vitals
    "note": "#6366f1",                  // Indigo/Purple-blue - documentation
    "observation": "#f97316",           // Orange - observations
    "procedure_occurrence": "#10b981",  // Green - procedures/treatments
    "specimen": "#ec4899",              // Pink - biological samples
    "visit_occurrence": "#eab308"       // Yellow - visits/encounters
  };

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

  html += '<div style="display: flex; gap: 48px; margin: 20px 0; align-items: flex-start;">';

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

  // Set up scroll tracking for sidebar navigation
  window.addEventListener("scroll", updateActiveNavOnScroll);
  updateActiveNavOnScroll();

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
// END OF JAVASCRIPT
// ============================================================================
  