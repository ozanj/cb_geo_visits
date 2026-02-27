$(function() {

  const modal = '<div class="modal"><span class="close">&times;</span><img /></div>';
  $('.slides section').prepend(modal);
  
  const link = '<a class="link" href="https://ozanj.github.io/cb_geo_visits/">ozanj.github.io/cb_geo_visits</a>';
  $('.slides section.level3').prepend(link);
  
  $('.slides section p img').on('click', function() {
    const src = $(this).attr('src'),
      width = $(this).attr('data-width');

    const $slide = $(this).closest('.slide');
    $slide.find('.modal img').attr('src', src);
    
    // reset width from any previous image
    $slide.find('.modal img').css('width', '');
    
    if (width) {
      $slide.find('.modal img').css('width', width);
    }
    
    $slide.find('.modal').fadeIn(600);
    $slide.find('p img').addClass('disabled');
  });
  
  $('.slides section .quote').on('click', function() {
    const $slide = $(this).closest('.slide');
    $slide.find('.quote-container').remove();
    
    const quote = $('<div class="quote-container"></div>').append($(this).next().html());
    
    $slide.find('.modal').append(quote);
    $slide.find('.modal').css('height', 'auto');
    
    $slide.find('.modal').fadeIn(600);
  })
  
  $('.close').on('click', function() {
    const $slide = $(this).closest('.slide');
    
    $slide.find('.modal').fadeOut(400, function() {
      $(this).css('height', '');
      $(this).find('.quote-container').remove();
    });
    $slide.find('p img').removeClass('disabled');
  });
  
  $('<hr>').insertAfter('.reveal .slide h4');
  $('<hr>').insertAfter('.reveal .slide:not(.caption) h3:not(:has(+ h4))');
  
  Reveal.addEventListener('slidechanged', function() {
    $('.modal').fadeOut(400, function() {
      $(this).css('height', '');
      $(this).find('.quote-container').remove();
    });
    $('img').removeClass('disabled');
  });
  
  Reveal.on('ready', (event) => {
    $('iframe').each(function() {
        $(this).attr('src', $(this).data('src'));
    });
  });
  
  $('.fullscreen').on('click', function() {
    $(this).prev('iframe').get(0).requestFullscreen();
  });
  
  const h3 = $('.slide:not(.hide) h3').map(function() {
    return $(this).text();
  }).get();

  $('.slide-menu-item-vertical').filter(function() {
    if (h3.includes($(this).text())) {
      $(this).css('padding-left', '42px');
    }
  });

});
