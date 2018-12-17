import Stickyfill from 'stickyfilljs'
import scrollama from 'scrollama'

import "./base.less"
import "./root.less"
import {Elm} from "./Main.elm"

import article from './article.json'

console.log(article)

const d3 = window.d3;

function initScrollama() {
    // using d3 for convenience
		var container = d3.select('#scroll');
		var graphic = container.select('.scroll__graphic');
		var chart = graphic.select('.chart');
		var text = container.select('.scroll__text');
		var step = text.selectAll('.step');
		// initialize the scrollama
		var scroller = scrollama();
		// generic window resize listener event
		function handleResize() {
			// 1. update height of step elements
			var stepHeight = Math.floor(window.innerHeight * 0.75);
			step.style('height', stepHeight + 'px');
			// 2. update width/height of graphic element
			var bodyWidth = d3.select('body').node().offsetWidth;
			var textWidth = text.node().offsetWidth;
			var graphicWidth = bodyWidth - textWidth;
			graphic
				.style('width', graphicWidth + 'px')
				.style('height', window.innerHeight + 'px');
			var chartMargin = 32;
			var chartWidth = graphic.node().offsetWidth - chartMargin;
			chart
				.style('width', chartWidth + 'px')
				.style('height', Math.floor(window.innerHeight / 2) + 'px');
			// 3. tell scrollama to update new element dimensions
			scroller.resize();
		}
		// scrollama event handlers
		function handleStepEnter(response) {
			// response = { element, direction, index }
			// add color to current step only
			step.classed('is-active', function (d, i) {
				return i === response.index;
			})
			// update graphic based on step
			chart.select('p').text(response.index + 1)
		}
		function handleContainerEnter(response) {
			// response = { direction }
		}
		function handleContainerExit(response) {
			// response = { direction }
		}
		function setupStickyfill() {
			d3.selectAll('.sticky').each(function () {
				Stickyfill.add(this);
			});
		}
		function init() {
			setupStickyfill();
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
				debug: true,
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
        var app = Elm.Main.init({
            node: document.getElementById('elm'),
            flags: { article }
          });
        this.fadeOut()
        initScrollama()
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
