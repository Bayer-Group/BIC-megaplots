# seriation module output updates correctly

    Code
      make_parameter_ui("id", var = "pain_intensity", name = "pain_intensity")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" for="id-pain_intensity_distmeasure">Distance Measure pain_intensity</label>
        <select data-live-search="true" data-header="Select item" id="id-pain_intensity_distmeasure" class="selectpicker form-control"><option value="OM" selected>OM</option>
      <option value="OMloc">OMloc</option>
      <option value="OMslen">OMslen</option>
      <option value="OMspell">OMspell</option>
      <option value="OMstran">OMstran</option>
      <option value="CHI2">CHI2</option>
      <option value="EUCLID">EUCLID</option>
      <option value="LCS">LCS</option>
      <option value="LCP">LCP</option>
      <option value="RLCP">RLCP</option>
      <option value="HAM">HAM</option>
      <option value="DHD">DHD</option></select>
      </div>
      <li class="treeview">
        <a href="#shiny-tab-parameters">
          <i class="fas fa-gears" role="presentation" aria-label="gears icon"></i>
          <span>
            <p style="white-space: pre-wrap; display: inline-block;">Parameters              </p>
          </span>
          <i class="fas fa-angle-left pull-right" role="presentation" aria-label="angle-left icon"></i>
        </a>
        <ul class="treeview-menu" style="display: none;" data-expanded="&lt;pstyle=&quot;white-space:pre-wrap;display:inline-block;&quot;&gt;Parameters&lt;/p&gt;">
          <div data-display-if="[&#39;OM&#39;, &#39;OMloc&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#39;OMstran&#39;, &#39;HAM&#39;,&#10;                           &#39;TWED&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_sm">Substitution Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_sm" class="selectpicker form-control"><option value="CONSTANT" selected>CONSTANT</option>
      <option value="INDELS">INDELS</option>
      <option value="INDELSLOG">INDELSLOG</option>
      <option value="ORDINAL">ORDINAL</option>
      <option value="TRATE">TRATE</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;DHD&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_smDHD">Substitution Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_smDHD" class="selectpicker form-control"><option value="INDELS" selected>INDELS</option>
      <option value="INDELSLOG">INDELSLOG</option>
      <option value="TRATE">TRATE</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;OM&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#10;                &#39;OMstran&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_indel">Insertion/Deletion Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_indel" class="selectpicker form-control"><option value="auto" selected>auto</option>
      <option value="numeric value">numeric value</option></select>
            </div>
            <div data-display-if="input[&#39;id-pain_intensity_indel&#39;] == &#39;numeric value&#39;" data-ns-prefix="">
              <div class="form-group shiny-input-container" style="width:150px;">
                <label class="control-label" id="id-pain_intensity_indel_numeric-label" for="id-pain_intensity_indel_numeric">Insertion/Deletion Cost (double)</label>
                <input id="id-pain_intensity_indel_numeric" type="number" class="form-control" value="1" min="0"/>
              </div>
            </div>
          </div>
          <div data-display-if="[&#39;OM&#39;, &#39;OMloc&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#10;                &#39;OMstran&#39;, &#39;DHD&#39;, &#39;LCS&#39;, &#39;LCP&#39;, &#39;RLCP&#39;&#10;                ].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_norm">Normalization</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_norm" class="selectpicker form-control"><option value="YujianBo">YujianBo</option>
      <option value="auto" selected>auto</option>
      <option value="gmean">gmean</option>
      <option value="maxdist">maxdist</option>
      <option value="maxlength">maxlength</option>
      <option value="none">none</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;CHI2&#39;, &#39;EUCLID&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_norm2">Normalization</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_norm2" class="selectpicker form-control"><option value="auto" selected>auto</option>
      <option value="none">none</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;OMloc&#39;,&#39;OMspell&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_expcost-label" for="id-pain_intensity_expcost">Cost of spell length transformation</label>
              <input id="id-pain_intensity_expcost" type="number" class="form-control" value="0.5" min="0" step="0.1"/>
            </div>
          </div>
          <div data-display-if="&#39;OMloc&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_context-label" for="id-pain_intensity_context">Local Insertion Cost</label>
              <input id="id-pain_intensity_context" type="number" class="form-control" value="0" min="0"/>
            </div>
          </div>
          <div data-display-if="&#39;OMslen&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_link">Substituion Costs Function</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_link" class="selectpicker form-control"><option value="gmean">gmean</option>
      <option value="mean" selected>mean</option></select>
            </div>
          </div>
          <div data-display-if="&#39;OMslen&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_h_OMslen-label" for="id-pain_intensity_h_OMslen">Exponential weight of spell length</label>
              <input id="id-pain_intensity_h_OMslen" type="number" class="form-control" value="0.5" min="0"/>
            </div>
          </div>
          <div data-display-if="&#39;OMspell&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_tpow-label" for="id-pain_intensity_tpow">Exponential weight of spell length</label>
              <input id="id-pain_intensity_tpow" type="number" class="form-control" value="1"/>
            </div>
          </div>
          <div data-display-if="&#39;OMstran&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_transindel">Transition Indel Cost Method</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_transindel" class="selectpicker form-control"><option value="constant" selected>constant</option>
      <option value="prob">prob</option>
      <option value="subcost">subcost</option></select>
            </div>
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_previous">Account for the transition from the previous state</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_previous" class="selectpicker form-control"><option value="FALSE" selected>FALSE</option>
      <option value="TRUE">TRUE</option></select>
            </div>
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_add.column">Duplicate the last column</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_add.column" class="selectpicker form-control"><option value="FALSE">FALSE</option>
      <option value="TRUE" selected>TRUE</option></select>
            </div>
          </div>
          <div data-display-if="&#39;OMstran&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_otto-label" for="id-pain_intensity_otto">Origin-Transition Trade-Off Weight</label>
              <input id="id-pain_intensity_otto" type="number" class="form-control" value="0.5" min="0" max="1" step="0.1"/>
            </div>
          </div>
          <div data-display-if="[&#39;CHI2&#39;, &#39;EUCLID&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_overlap">Intervals overlapping</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_overlap" class="selectpicker form-control"><option value="FALSE" selected>FALSE</option>
      <option value="TRUE">TRUE</option></select>
            </div>
            <div data-display-if="&#39;TRUE&#39; == input[&#39;id-pain_intensity_overlap&#39;]" data-ns-prefix="">
              <span style="color:#e6710b">The interval length must be an<br/>even number</span>
              <br/>
            </div>
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-pain_intensity_step-label" for="id-pain_intensity_step">Interval Length</label>
              <input id="id-pain_intensity_step" type="number" class="form-control" value="1" min="1" step="1"/>
            </div>
          </div>
          <div data-display-if="&#39;CHI2&#39; == input[&#39;id-pain_intensity_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-pain_intensity_weighted">Distribution of states as weights</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-pain_intensity_weighted" class="selectpicker form-control"><option value="FALSE">FALSE</option>
      <option value="TRUE" selected>TRUE</option></select>
            </div>
          </div>
          <div class="form-group shiny-input-container">
            <label class="control-label" for="id-pain_intensity_methMissing">Missing method</label>
            <select data-live-search="true" data-header="Select item" id="id-pain_intensity_methMissing" class="selectpicker form-control"><option value="new state" selected>new state</option>
      <option value="last observed state">last observed state</option></select>
          </div>
        </ul>
      </li>
      <div data-display-if="[&#39;HAM&#39;, &#39;DHD&#39;].indexOf(input[&#39;id-pain_intensity_distmeasure&#39;]) !== -1" data-ns-prefix="">
        <span style="color:#e6250b">This distance measure only<br/>works for sequences of the<br/>same length!</span>
        <br/>
      </div>

---

    Code
      make_parameter_ui("id", var = "rm", name = "")
    Output
      <div class="form-group shiny-input-container">
        <label class="control-label" for="id-rm_distmeasure">Distance Measure </label>
        <select data-live-search="true" data-header="Select item" id="id-rm_distmeasure" class="selectpicker form-control"><option value="OM" selected>OM</option>
      <option value="OMloc">OMloc</option>
      <option value="OMslen">OMslen</option>
      <option value="OMspell">OMspell</option>
      <option value="OMstran">OMstran</option>
      <option value="CHI2">CHI2</option>
      <option value="EUCLID">EUCLID</option>
      <option value="LCS">LCS</option>
      <option value="LCP">LCP</option>
      <option value="RLCP">RLCP</option>
      <option value="HAM">HAM</option>
      <option value="DHD">DHD</option></select>
      </div>
      <li class="treeview">
        <a href="#shiny-tab-parameters">
          <i class="fas fa-gears" role="presentation" aria-label="gears icon"></i>
          <span>
            <p style="white-space: pre-wrap; display: inline-block;">Parameters              </p>
          </span>
          <i class="fas fa-angle-left pull-right" role="presentation" aria-label="angle-left icon"></i>
        </a>
        <ul class="treeview-menu" style="display: none;" data-expanded="&lt;pstyle=&quot;white-space:pre-wrap;display:inline-block;&quot;&gt;Parameters&lt;/p&gt;">
          <div data-display-if="[&#39;OM&#39;, &#39;OMloc&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#39;OMstran&#39;, &#39;HAM&#39;,&#10;                           &#39;TWED&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_sm">Substitution Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_sm" class="selectpicker form-control"><option value="CONSTANT" selected>CONSTANT</option>
      <option value="INDELS">INDELS</option>
      <option value="INDELSLOG">INDELSLOG</option>
      <option value="ORDINAL">ORDINAL</option>
      <option value="TRATE">TRATE</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;DHD&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_smDHD">Substitution Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_smDHD" class="selectpicker form-control"><option value="INDELS" selected>INDELS</option>
      <option value="INDELSLOG">INDELSLOG</option>
      <option value="TRATE">TRATE</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;OM&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#10;                &#39;OMstran&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_indel">Insertion/Deletion Cost</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_indel" class="selectpicker form-control"><option value="auto" selected>auto</option>
      <option value="numeric value">numeric value</option></select>
            </div>
            <div data-display-if="input[&#39;id-rm_indel&#39;] == &#39;numeric value&#39;" data-ns-prefix="">
              <div class="form-group shiny-input-container" style="width:150px;">
                <label class="control-label" id="id-rm_indel_numeric-label" for="id-rm_indel_numeric">Insertion/Deletion Cost (double)</label>
                <input id="id-rm_indel_numeric" type="number" class="form-control" value="1" min="0"/>
              </div>
            </div>
          </div>
          <div data-display-if="[&#39;OM&#39;, &#39;OMloc&#39;, &#39;OMslen&#39;, &#39;OMspell&#39;,&#10;                &#39;OMstran&#39;, &#39;DHD&#39;, &#39;LCS&#39;, &#39;LCP&#39;, &#39;RLCP&#39;&#10;                ].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_norm">Normalization</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_norm" class="selectpicker form-control"><option value="YujianBo">YujianBo</option>
      <option value="auto" selected>auto</option>
      <option value="gmean">gmean</option>
      <option value="maxdist">maxdist</option>
      <option value="maxlength">maxlength</option>
      <option value="none">none</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;CHI2&#39;, &#39;EUCLID&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_norm2">Normalization</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_norm2" class="selectpicker form-control"><option value="auto" selected>auto</option>
      <option value="none">none</option></select>
            </div>
          </div>
          <div data-display-if="[&#39;OMloc&#39;,&#39;OMspell&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_expcost-label" for="id-rm_expcost">Cost of spell length transformation</label>
              <input id="id-rm_expcost" type="number" class="form-control" value="0.5" min="0" step="0.1"/>
            </div>
          </div>
          <div data-display-if="&#39;OMloc&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_context-label" for="id-rm_context">Local Insertion Cost</label>
              <input id="id-rm_context" type="number" class="form-control" value="0" min="0"/>
            </div>
          </div>
          <div data-display-if="&#39;OMslen&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_link">Substituion Costs Function</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_link" class="selectpicker form-control"><option value="gmean">gmean</option>
      <option value="mean" selected>mean</option></select>
            </div>
          </div>
          <div data-display-if="&#39;OMslen&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_h_OMslen-label" for="id-rm_h_OMslen">Exponential weight of spell length</label>
              <input id="id-rm_h_OMslen" type="number" class="form-control" value="0.5" min="0"/>
            </div>
          </div>
          <div data-display-if="&#39;OMspell&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_tpow-label" for="id-rm_tpow">Exponential weight of spell length</label>
              <input id="id-rm_tpow" type="number" class="form-control" value="1"/>
            </div>
          </div>
          <div data-display-if="&#39;OMstran&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_transindel">Transition Indel Cost Method</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_transindel" class="selectpicker form-control"><option value="constant" selected>constant</option>
      <option value="prob">prob</option>
      <option value="subcost">subcost</option></select>
            </div>
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_previous">Account for the transition from the previous state</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_previous" class="selectpicker form-control"><option value="FALSE" selected>FALSE</option>
      <option value="TRUE">TRUE</option></select>
            </div>
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_add.column">Duplicate the last column</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_add.column" class="selectpicker form-control"><option value="FALSE">FALSE</option>
      <option value="TRUE" selected>TRUE</option></select>
            </div>
          </div>
          <div data-display-if="&#39;OMstran&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_otto-label" for="id-rm_otto">Origin-Transition Trade-Off Weight</label>
              <input id="id-rm_otto" type="number" class="form-control" value="0.5" min="0" max="1" step="0.1"/>
            </div>
          </div>
          <div data-display-if="[&#39;CHI2&#39;, &#39;EUCLID&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_overlap">Intervals overlapping</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_overlap" class="selectpicker form-control"><option value="FALSE" selected>FALSE</option>
      <option value="TRUE">TRUE</option></select>
            </div>
            <div data-display-if="&#39;TRUE&#39; == input[&#39;id-rm_overlap&#39;]" data-ns-prefix="">
              <span style="color:#e6710b">The interval length must be an<br/>even number</span>
              <br/>
            </div>
            <div class="form-group shiny-input-container" style="width:150px;">
              <label class="control-label" id="id-rm_step-label" for="id-rm_step">Interval Length</label>
              <input id="id-rm_step" type="number" class="form-control" value="1" min="1" step="1"/>
            </div>
          </div>
          <div data-display-if="&#39;CHI2&#39; == input[&#39;id-rm_distmeasure&#39;]" data-ns-prefix="">
            <div class="form-group shiny-input-container" style="width: 150px;">
              <label class="control-label" for="id-rm_weighted">Distribution of states as weights</label>
              <select data-live-search="true" data-header="Select item" data-width="150" id="id-rm_weighted" class="selectpicker form-control"><option value="FALSE">FALSE</option>
      <option value="TRUE" selected>TRUE</option></select>
            </div>
          </div>
          <div class="form-group shiny-input-container">
            <label class="control-label" for="id-rm_methMissing">Missing method</label>
            <select data-live-search="true" data-header="Select item" id="id-rm_methMissing" class="selectpicker form-control"><option value="new state" selected>new state</option>
      <option value="last observed state">last observed state</option></select>
          </div>
        </ul>
      </li>
      <div data-display-if="[&#39;HAM&#39;, &#39;DHD&#39;].indexOf(input[&#39;id-rm_distmeasure&#39;]) !== -1" data-ns-prefix="">
        <span style="color:#e6250b">This distance measure only<br/>works for sequences of the<br/>same length!</span>
        <br/>
      </div>

---

    Code
      seriation_ui("id")
    Output
      <div id="id-parameters" class="shiny-html-output"></div>

