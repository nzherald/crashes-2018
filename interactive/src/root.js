import 'intersection-observer'
import scrollama from 'scrollama'
import stickybits from 'stickybits'

import "./base.less"
import "./root.less"
import {Elm} from "./Main.elm"

import article from './article.json'
import nym from './crash+nym_yearly.json'
import periods from './crash+xmas_periods.json'
import trends from './crash+daily_trend.json'


function initScrollama(app) {
    // const {default: scrollama} = await import ('scrollama')
    // const {default: Stickyfill} = await import ('stickyfilljs')
		var container = document.getElementById('scroll');
		var graphic = container.children[0]
		var text = container.children[1]
		var chart = graphic.children[0]
		var steps = text.children
		// initialize the scrollama
		var scroller = scrollama();
		// generic window resize listener event
		var stepFactor = window.innerWidth >= 600 ? 0.75 : 1.75;
		var chartFactor = window.innerWidth >= 600 ? 0.9 : 1;
		var small = window.innerWidth < 600;
		function handleResize() {
			// 1. update height of step elements
            var stepHeight = Math.floor(window.innerHeight * stepFactor);
            for (var i = 0; i < steps.length; i++) {
                steps[i].style.height = stepHeight + 'px'
            }
			// 2. update width/height of graphic element
			var bodyWidth = document.getElementById('root').offsetWidth
			var textWidth = text.offsetWidth;
			var graphicWidth = bodyWidth - textWidth;
            graphic.style['width'] = small ? "100%" : graphicWidth + 'px'
            graphic.style['height'] = (window.innerHeight - 60) + 'px'
            graphic.style['top'] = "60px"
			var chartMargin = 32;
			var chartWidth = graphic.offsetWidth - chartMargin;
			chart.style['width'] = small ? "100%" : chartWidth + 'px'
			chart.style['height'] = Math.floor(window.innerHeight * chartFactor) + 'px'
			// 3. tell scrollama to update new element dimensions
			scroller.resize();
		}
		// scrollama event handlers
		function handleStepEnter(response) {
			// response = { element, direction, index }
            // update graphic based on step
            app.ports.scroll.send([response.element.attributes['data-label'].value, response.index])
		}
		function handleContainerEnter(response) {
			// response = { direction }
		}
		function handleContainerExit(response) {
			// response = { direction }
		}
		function init() {
            stickybits(document.querySelectorAll('.sticky'))
			// 1. force a resize on load to ensure proper dimensions are sent to scrollama
			handleResize();
			// 2. setup the scroller passing options
			// this will also initialize trigger observations
			// 3. bind scrollama event handlers (this can be chained like below)
			scroller.setup({
				container: '#scroll',
				graphic: '.scroll__graphic',
				text: '.scroll__text',
				step: '.scroll__text .step',
				offset: 0.5,
				debug: false,
			})
				.onStepEnter(handleStepEnter)
				.onContainerEnter(handleContainerEnter)
				.onContainerExit(handleContainerExit);
			// setup resize event
			window.addEventListener('resize', handleResize);
		}
		// kick things off
		init();
}

class Main {
    constructor () {
		document.getElementById('root').innerHTML = '<div id="elm"></div>'
		const { width } =document.getElementById('root').getClientRects()[0] 
        var app = Elm.Main.init({
            node: document.getElementById('elm'),
            flags: { article, nym, periods, trends, small: window.innerWidth < 900, width }
          });
        this.fadeOut()
        initScrollama(app)
    }

    fadeOut (b) {
        sessionStorage.setItem("loading", "done");
        if (typeof($) !== "undefined") {
            $("#loading").fadeTo(600, 0.01, () => {
                $("#loading").remove()
                console.log("Loading screen removed.")
                if (b) b()
            })
        } else {
            var loadingRemove = document.getElementById("loading")
            if (loadingRemove) {
                document.body.removeChild(loadingRemove)
                console.log("Loading screen removed.")
            }
            if (b) b()
        }
    }
}

new Main()
