function RelatedStandards(){
	//https://www.w3schools.com/bootstrap4/tryit.asp?filename=trybs_ref_js_carousel_js

	// https://meyerweb.com/eric/tools/css/reset/
	// var meyer_reset = ".css-reset{ html, body, div, span, applet, object, iframe, h1, h2, h3, h4, h5, h6, p, blockquote, pre, a, abbr, acronym, address, big, cite, code, del, dfn, em, img, ins, kbd, q, s, samp, small, strike, strong, sub, sup, tt, var, b, u, i, center, dl, dt, dd, ol, ul, li, fieldset, form, label, legend, table, caption, tbody, tfoot, thead, tr, th, td, article, aside, canvas, details, embed, figure, figcaption, footer, header, hgroup, menu, nav, output, ruby, section, summary, time, mark, audio, video { margin: 0; padding: 0; border: 0; font-size: 100%; font: inherit; vertical-align: baseline; } /* HTML5 display-role reset for older browsers */ article, aside, details, figcaption, figure, footer, header, hgroup, menu, nav, section { display: block; } body { line-height: 1; } ol, ul { list-style: none; } blockquote, q { quotes: none; } blockquote:before, blockquote:after, q:before, q:after { content: ''; content: none; } table { border-collapse: collapse; border-spacing: 0; }}";

	//document.head
	// var bootstrap_link = document.createElement("link");
	// bootstrap_link["rel"] = "stylesheet";
	// bootstrap_link["href"] = "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css";
	// var script_jquery = document.createElement("script");
	// script_jquery.type = 'text/javascript';
	// script_jquery.src = "https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js";
	// var script_popper = document.createElement("script");
	// script_popper.type = 'text/javascript';
	// script_popper.src = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.16.0/umd/popper.min.js";
	// var script_bootstrap = document.createElement("script");
	// script_bootstrap.type = 'text/javascript';
	// script_bootstrap.src = "https://maxcdn.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js";
	// var css = ".carousel-inner img { width: 100%; height: 100%; } .gray-bg { background-color: lightgray; } .black { color: black; }";
	// css += meyer_reset;
	// var css_element = document.createElement("script");
	// css_element.type = "text/css";
	// if (css_element.styleSheet){
	// 	// This is required for IE8 and below.
	// 	css_element.styleSheet.cssText = css;
	// } else {
	// 	css_element.appendChild(document.createTextNode(css));
	// }
	// var head = document.head || document.getElementsByTagName('head')[0];
	// head.appendChild(bootstrap_link);
	// head.appendChild(script_jquery);
	// head.appendChild(script_popper);
	// head.appendChild(script_bootstrap);
	// head.appendChild(css_element);

	//carousel container
	var container = document.createElement("div");
	container.className = "css-reset container mt-3";
	// var indicators = document.createElement("ul");
	var indicators = document.createElement("h5");
	indicators.appendChild(document.createTextNode('Other Users Also Bought'));
	// indicators.className = "carousel-indicators";

	//carousel buttons
	// var item1 = document.createElement("li");
	// item1.className = "item1 black active";
	// indicators.appendChild(item1);
	// var item2 = document.createElement("li");
	// item2.className = "item2 black";
	// indicators.appendChild(item2);
	// var item3 = document.createElement("li");
	// item3.className = "item3 black";
	// indicators.appendChild(item3);
	container.appendChild(indicators);

	//carousel content
	var carousel_inner = document.createElement("div");
	var standard1 = document.createElement("div");
	var standard1_text = document.createElement("a");
	standard1_text.appendChild(document.createTextNode('ANSI A137.1:2017'));
	standard1_text["href"] = "https://webstore.ansi.org/Standards/TCA/ANSIA1372017";
	standard1.appendChild(standard1_text);
	// standard1.className = "gray-bg";
	// standard1.width = "1100";
	// standard1.height = "500";
	var standard2 = document.createElement("div");
	var standard2_text = document.createElement("a");
	standard2_text.appendChild(document.createTextNode('ANSI A137.1:2012'));
	standard2_text["href"] = "https://webstore.ansi.org/Standards/TCA/ANSIA1372012";
	standard2.appendChild(standard2_text);
	// standard2.className = "gray-bg";
	// standard2.width = "1100";
	// standard2.height = "500";
	var standard3 = document.createElement("div");
	var standard3_text = document.createElement("a");
	standard3_text.appendChild(document.createTextNode('ANSI A108/A118/A136.1:2014'));
	standard3_text["href"] = "https://webstore.ansi.org/Standards/TCA/ANSIA108A118A1362014";
	standard3.appendChild(standard3_text);
	// standard3.className = "gray-bg";
	// standard3.width = "1100";
	// standard3.height = "500";
	carousel_inner.appendChild(standard1);
	carousel_inner.appendChild(standard2);
	carousel_inner.appendChild(standard3);
	container.appendChild(carousel_inner);

	var prev_button = document.createElement("a");
	prev_button.className = "carousel-control-prev";
	prev_button["href"] = "#myCarousel";
	var prev_span = document.createElement("span");
	prev_span.className = "carousel-control-prev-icon";
	prev_button.appendChild(prev_span);
	// container.appendChild(prev_button);

	var next_button = document.createElement("a");
	next_button.className = "carousel-control-next";
	next_button["href"] = "#myCarousel";
	var next_span = document.createElement("span");
	next_button.className = "carousel-control-next-icon";
	next_button.appendChild(next_span);
	// container.appendChild(next_button);
	//carousel container end

	var jquery = document.createElement("script");
	jquery.appendChild(document.createTextNode('$(document).ready(function(){ $("#myCarousel").carousel(); $(".item1").click(function(){ $("#myCarousel").carousel(0); }); $(".item2").click(function(){ $("#myCarousel").carousel(1); }); $(".item3").click(function(){ $("#myCarousel").carousel(2); }); $(".carousel-control-prev").click(function(){ $("#myCarousel").carousel("prev"); }); $(".carousel-control-next").click(function(){ $("#myCarousel").carousel("next"); }); });'));

	var target_location = document.getElementsByClassName("col-lg-7 g-mb-20 g-pr-0");
	target_location[0].appendChild(container);
	// target_location[0].appendChild(jquery);
}

RelatedStandards();