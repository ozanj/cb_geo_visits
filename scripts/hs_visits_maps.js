function(el, x, choices) {

  let myMap = this;
  
  let region = choices.region_choices.region,
    region_name = choices.region_choices.region_name,
    univ_abbrev = choices.univ_vars.abbrev,
    univ_name = choices.univ_vars.name,
    market_abbrev = choices.market_vars.abbrev,
    market_name = choices.market_vars.name;
      
  $(String.raw`<style>.collapsible-label span::before { content: "➕"; margin-right: 2.5px; margin-left: 5.5px; width: 13px; display: inline-block; transition: transform 0.25s ease-out; } .collapsible-label.active span::before { content: "➖" } .collapsible-label:not(.active) span::before { transform: rotate(90deg); } .leaflet-control-layers-base { width: 230px; } .leaflet-popup-content ul { margin: 0; padding: 0 15px; } .leaflet-popup-content p { border-left: 2px lightgray solid; color: gray; padding: 0 7px; margin: 2px 5px; }</style>`).appendTo('head');

  $('.leaflet-control-layers-base').prepend('<p style="margin: 5px; font-weight: 600;">Census data (by tract)</p>');
  $('.leaflet-control-layers-overlays').prepend('<p style="margin: 8px 5px 5px; font-weight: 600;">Recruiting visits (by school)</p>');

  // race/ethnicity base layer
  
  let raceOptions = $('.leaflet-control-layers-base label').filter(function() {
    return $(this).text().trim().startsWith('%');
  });
  
  raceControlHTML = '<label class="collapsible-label" data-label="race" style="cursor: pointer;"><div><span style="font-weight: 500;"> MSA by Race/Ethnicity</span></div></label><div id="race-container" style="padding-left: 20px;"></div>';
  
  $('.leaflet-control-layers-base label:nth-child(3)').after(raceControlHTML);
  
  $('#race-container').append(raceOptions).slideUp(0);
  
  // distance selection options
  
  let distanceControlHTML = '<p style="margin: 5px; font-weight: 600;">Border distance</p><div style="display: flex; margin-bottom: 5px;">';
  
  const distanceValues = ['NA', '2', '1', 'half'];
  
  ['NA', '<2 mi', '<1 mi', '<0.5 mi'].forEach(function(curr, idx) {
    distanceControlHTML += '<div' + (idx === 0 ? '' : ' style="margin-left: 5px;"') + '><input type="radio" class="leaflet-control-layers-selector" name="distance-choice" data-distance="' + distanceValues[idx] + '"><span> ' + curr + '</span></div>';
  });
  
  distanceControlHTML += '</div>';
  
  $('.leaflet-control-layers-base').append(distanceControlHTML);
  
  // visited hs overlay layer
  
  let univHSControlHTML = '<label class="collapsible-label" data-label="hs-univ" style="cursor: pointer; margin-left: -1px;"><div><span style="font-weight: 500;"> by Visiting Universities</span></div></label><div id="hs-univ-container" style="padding-left: 19px;">';
  
  univ_name.forEach(function(curr, idx) {
    univHSControlHTML += '<label style="margin-left: ' + (['Private National', 'Private Liberal Arts'].includes(curr) ? '19px' : '0') + ';"><div><input type="radio" class="leaflet-control-layers-selector" name="univ-choice" data-univ="' + univ_abbrev[idx] + '"><span> ' + curr + '</span></div></label>'
  });
  
  univHSControlHTML += '</div>';
  
  let marketHSControlHTML = '<label class="collapsible-label" data-label="hs-market" style="cursor: pointer; margin-left: -1px;"><div><span style="font-weight: 500;"> by Visit Market</span></div></label><div id="hs-market-container" style="padding-left: 19px;">';
  
  market_name.forEach(function(curr, idx) {
    marketHSControlHTML += '<label style="margin-left: ' + (['Local', 'In-state', 'Regional', 'National'].includes(curr) ? '19px' : '0') + ';"><div><input type="radio" class="leaflet-control-layers-selector" name="market-choice" data-market="' + market_abbrev[idx] + '"><span> ' + curr + '</span></div></label>'
  });
  
  marketHSControlHTML += '</div>';
  
  $('.leaflet-control-layers-overlays').append(univHSControlHTML + marketHSControlHTML);
  
  $('#hs-univ-container').slideUp(0);
  $('#hs-market-container').slideUp(0);
  
  // label toggle button
  let labelHTML = '<div style="height: 0; border-top: 1px solid #ddd; margin: 5px -10px 5px -6px;"></div><div><input type="checkbox" class="leaflet-control-layers-selector" id="label-toggle"><span> Show EPS labels</span></div>'
  
  $('.leaflet-control-layers-overlays').append(labelHTML);
  
  // handle collapsible selection
  
  $('.collapsible-label').on('click', function(e) {
    $(this).toggleClass('active');
    $('#' + $(this).attr('data-label') + '-container').slideToggle();
  })
  
  // selection text
  
  selTextHTML = '<div id="selection-text" style="padding: 10px; display: inline-block; font-weight: 900; color: #444;"></div>';
  
  $('.leaflet > .leaflet-control-container > .leaflet-top.leaflet-left').append(selTextHTML);
  
  // handle selections
  $('input[name="distance-choice"]').on('change', function(e) {
    let $this = $(this);
    
    let distance = $this.attr('data-distance');
    
    active_attr.active_distance = distance;
    update_base_layer();
  });
  
  $('input[name="univ-choice"]').on('change', function(e) {
    active_attr.active_univ_type = $(this).attr('data-univ');
    update_pins();
  });
  
  $('input[name="market-choice"]').on('change', function(e) {
    active_attr.active_market_type = $(this).attr('data-market');
    update_pins();
  });
  
  $('#label-toggle').on('change', function() {
    $('.label').css('opacity', $(this).is(':checked') ? 100 : 0);
  })
  
  let update_base_layer = function() {
    $('.metro-shape').css('display', 'none');
    
    $('.metro-shape.metro-' + active_attr.active_metro).css('display', 'inherit');
    
    if (active_attr.active_base !== 'MSA') {
      $('.metro-line-' + active_attr.active_metro).css('display', 'inherit');
    }
    
    $('.metro-distance').css('display', 'none');

    if (active_attr.active_distance !== 'NA') {
      $('.metro-distance-' + active_attr.active_distance).css('display', 'inherit');
    }
  };
  
  let update_pins = function() {
    $('.hs-pin').css('display', 'none');
    $('input[name="univ-choice"], input[name="market-choice"]').prop('disabled', false);
    
    for (abbrev of market_abbrev) {
      if ($('.hs-pin.hs-' + active_attr.active_univ_type + '-' + abbrev).length === 0) {
        $('input[name="market-choice"][data-market="' + abbrev + '"]').prop('disabled', true);
      }
    }
    for (abbrev of univ_abbrev) {
      if ($('.hs-pin.hs-' + abbrev + '-' + active_attr.active_market_type).length === 0) {
        $('input[name="univ-choice"][data-univ="' + abbrev + '"]').prop('disabled', true);
      }
    }

    $('.hs-pin.hs-' + active_attr.active_univ_type + '-' + active_attr.active_market_type).css('display', 'inherit');
  }
  
  // handle selection text update
  
  let update_sel_text = function() {
    let sel_metro = region_name[region.indexOf(active_attr.active_metro)] // $('input[data-region="' + active_attr.active_metro + '"]').next().text();
    
    if (active_attr.active_order !== '') {
      sel_metro += ' - ' + $('input[data-order="' + active_attr.active_order + '"]').next().text();
    }
    
    $('#selection-text').text(sel_metro);
  };
  
  // handle legend update
  
  let update_legend = function(sel_name) {
    $('.legend').css('display', 'none');
    switch (active_attr.active_base) {
      case 'MSA by Total Population':
        $('.legend-pop-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by Median Income':
        $('.legend-income-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% White, non-Hispanic':
        $('.legend-nhisp_white-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Black, non-Hispanic':
        $('.legend-nhisp_black-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Hispanic':
        $('.legend-hisp_all-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% Asian, non-Hispanic':
        $('.legend-nhisp_asian-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% NHPI, non-Hispanic':
        $('.legend-nhisp_nhpi-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% AIAN, non-Hispanic':
        $('.legend-nhisp_native-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case '% 2+ Races, non-Hispanic':
        $('.legend-nhisp_multi-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by % in Poverty':
        $('.legend-pov-' + active_attr.active_metro).css('display', 'inherit');
        break;
      case 'MSA by % with BA+':
        $('.legend-edu-' + active_attr.active_metro).css('display', 'inherit');
        break;
    }
  };
  
  // handle controls selection

  myMap.on('baselayerchange', function(e) {
    active_attr.active_base = e.name;
    update_legend();
    
    e.layer.bringToBack();
    update_base_layer();
  });
  
  myMap.on('overlayadd', function(e) {
    update_pins();
  });
  
  // default settings on load
      
  let active_attr = {
    active_base: 'MSA',
    active_metro: region[0],
    active_distance: 'NA',
    active_univ_type: 'all',
    active_market_type: 'all'
  };
  
  $('.legend').css('display', 'none');
  $('.label').css('opacity', 0);
  
  $('input[data-distance="' + active_attr.active_distance + '"]').trigger('click');
  
  $('input[data-univ="' + active_attr.active_univ_type + '"]').trigger('click');
  $('input[data-market="' + active_attr.active_market_type + '"]').trigger('click');
  
  // update_sel_text();
}
