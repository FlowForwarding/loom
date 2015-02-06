<a name="v3.0.0-rc.16"></a>
### v3.0.0-rc.16 (2014-11-13)


#### Bug Fixes

* **Edit:** Retain "focus" in endEdit() ([f3a45ef5](http://github.com/angular-ui/ng-grid/commit/f3a45ef51e5f49c11490654cb757b579163e45ba))
* **Filter:** Watch running w/o change check ([79f6c21a](http://github.com/angular-ui/ng-grid/commit/79f6c21a45fe11e4ae87f3dd6eda8326bcfbb265))
* **Grid:** Avoid too-early header height calc ([a335b922](http://github.com/angular-ui/ng-grid/commit/a335b9223c397dca31d5db82f48d62e9b7b43187))
* **GridRenderContainer:** Correct prevScroll* ([1182b059](http://github.com/angular-ui/ng-grid/commit/1182b059ce64b3b5b4e49054b4737684393e5a19))
* **Selection:** Change api for getSelectAllState() ([5ffefafc](http://github.com/angular-ui/ng-grid/commit/5ffefafc000351a71b269ce2265f2faddb35b6e7), closes [#2086](http://github.com/angular-ui/ng-grid/issues/2086))
* **cellNav:**
  * Don't lose focus with wrapping nav ([0e27c181](http://github.com/angular-ui/ng-grid/commit/0e27c1815d9fe1aabce2a9ba981bb75655f08979))
  * Minor fixes and speed improvement ([94a31149](http://github.com/angular-ui/ng-grid/commit/94a31149ca0282612d7dc85d87d2a68f2c12367c))
* **gridUtil:** Allow multiple logDebug params ([c903ecc8](http://github.com/angular-ui/ng-grid/commit/c903ecc89fabebceed576f4ca637bb3c5277a32c))
* **uiGrid:**
  * syntax error, change variable name ([63fedb6c](http://github.com/angular-ui/ng-grid/commit/63fedb6c87ac7b60d3362ce3f9359fdf070848ed))
  * syntax error, change variable name ([181dec76](http://github.com/angular-ui/ng-grid/commit/181dec76aed49ff03a7f2c329ae8b6d65bdd594e))
  * Fix race condition in data watcher ([2b25f249](http://github.com/angular-ui/ng-grid/commit/2b25f249cd133a02f44eec34ccee7f4e77adab16), closes [#2053](http://github.com/angular-ui/ng-grid/issues/2053))
* **uiGridMenu:** Only run applyHide when not shown ([91bf06f2](http://github.com/angular-ui/ng-grid/commit/91bf06f289632f331fd125851282cb9f5b499bd4))

<a name="v3.0.0-rc.15"></a>
### v3.0.0-rc.15 (2014-11-11)


#### Bug Fixes

* **uiGridRenderContainer:** Inherit position ([fd353287](http://github.com/angular-ui/ng-grid/commit/fd353287639276083340ef96aa82749367f1deef), closes [#2030](http://github.com/angular-ui/ng-grid/issues/2030))


#### Features

* **Interpolation:** Handle custom symbols ([555ddecd](http://github.com/angular-ui/ng-grid/commit/555ddecddeb967f338f8bdf4da814d74f8b44495), closes [#1576](http://github.com/angular-ui/ng-grid/issues/1576))

<a name="v3.0.0-rc.14"></a>
### v3.0.0-rc.14 (2014-11-05)


#### Bug Fixes

* **bower:** Use right angular version spec ([6bd917a0](http://github.com/angular-ui/ng-grid/commit/6bd917a05527afe7f75db51bdaff24b44f554fd9))

<a name="v3.0.0-rc.13"></a>
### v3.0.0-rc.13 (2014-11-05)


#### Bug Fixes

* **AutoResize:** Redraw all of grid on resize. ([1bb5367c](http://github.com/angular-ui/ng-grid/commit/1bb5367cb304baeee6ca4ecc084cbc02601fb6f6), closes [#1770](http://github.com/angular-ui/ng-grid/issues/1770))
* **Importer:** Account for older browsers properly ([52823249](http://github.com/angular-ui/ng-grid/commit/528232492ea9860390dd8da1a604103b25f33719))

<a name="v3.0.0-rc.12"></a>
### v3.0.0-rc.12 (2014-10-08)


#### Bug Fixes

* **Customizer:** Update column menu style ([ffa80002](http://github.com/angular-ui/ng-grid/commit/ffa80002c983b5eb60a4357ba2d7150682f343a2))
* **Grid:** Fix a few mobile-browser issues ([4bb2d699](http://github.com/angular-ui/ng-grid/commit/4bb2d6996aad6463155bcd01b1521441cab45270))
* **Pinning:** Fix pinning when col has dyanmic width ([9e022bab](http://github.com/angular-ui/ng-grid/commit/9e022babd07f6261ce6b4325b791c744da895f91), closes [#1634](http://github.com/angular-ui/ng-grid/issues/1634))
* **RTL:** Unfixed .css() call post-jquery ([8d31f6a1](http://github.com/angular-ui/ng-grid/commit/8d31f6a1a2ae68d9c9b40fa6a2bcd3ed47febe96), closes [#1620](http://github.com/angular-ui/ng-grid/issues/1620))
* **Selection:** Remove IE11 ellipsis in header ([a8ac76c6](http://github.com/angular-ui/ng-grid/commit/a8ac76c638c65ef3c9a062f9efb7847ebc5ddd94))
* **Site:** Fix version select box height ([d2699092](http://github.com/angular-ui/ng-grid/commit/d2699092b0fd882c6ed2999595d5805854fa8749))
* **Tests:** Date() usage failing on Safari 5 ([6440d81c](http://github.com/angular-ui/ng-grid/commit/6440d81c6d2ca2604aff5e0f8904a6e66093b8a9))
* **Travis:** Make travis run saucelabs tests ([3bc3f272](http://github.com/angular-ui/ng-grid/commit/3bc3f272986c2cae55f26bc6ef8dd8f5fa6ef401))
* **grunt:** Use correct version of grunt-jscs ([e88600a0](http://github.com/angular-ui/ng-grid/commit/e88600a0118af1535be2e4f66c0e09c41aa15efa))
* **uiGridCell:** Use promises for tmpl processing ([9fb29cce](http://github.com/angular-ui/ng-grid/commit/9fb29cce5ea5f67c43d2aa5926a4f7a8aa4ba81c), closes [#1712](http://github.com/angular-ui/ng-grid/issues/1712))
* **uiGridHeader:** Fix height calculations ([cfc24442](http://github.com/angular-ui/ng-grid/commit/cfc24442e08d6e9956c7f94159ddf30b7db185f7), closes [#1639](http://github.com/angular-ui/ng-grid/issues/1639), [#NaN](http://github.com/angular-ui/ng-grid/issues/NaN))
* **uiGridHeaderCell:** Use hasClass() for IE9 ([7923f229](http://github.com/angular-ui/ng-grid/commit/7923f229a4657538923a535bb7bd5d501320c3b8))
* **uiGridMenu:** Set box-sizing: content box ([ee418f0d](http://github.com/angular-ui/ng-grid/commit/ee418f0de9c6bf5c727d7eed7daa153dce1c5dd1))
* **uiGridUtil:** getBorderSize() missing "width" ([174f2521](http://github.com/angular-ui/ng-grid/commit/174f25214caa10ec643db6c81aaa0f3511bf78f4))


#### Features

* **cellTemplates:** add MODEL_COL_FIELD to cellTemplate parsing so it can be used in ng-model #1633  ([66f3404a](http://github.com/angular-ui/ng-grid/commit/66f3404ade7ead010142ecf047e863f526d960a3))


<a name="v3.0.0-rc.NEXT"></a>
### v3.0.0-rc.NEXT Current Master

#### Breaking Changes
* editableCellTemplates should use MODEL_COL_FIELD instead of COL_FIELD.
https://github.com/angular-ui/ng-grid/issues/1633 MODEL_COL_FIELD variable was added to the cellTemplate and editCellTemplate for utilizing the bound field in ng-model.  Edit feature
was changed to use MODEL_COL_FIELD instead of COL_FIELD for consistency.


<a name="v3.0.0-rc.11"></a>
### v3.0.0-rc.11 (2014-09-26)

<a name="v3.0.0-rc.10"></a>
### v3.0.0-rc.10 (2014-09-25)


#### Bug Fixes

* typo ([1272cf65](http://github.com/angular-ui/ng-grid/commit/1272cf65659a0e84102be9b1fa63f659437383ca))
* **CHANGELOG:** anchor tag for v2.0.7 was wrong ([d0ecd310](http://github.com/angular-ui/ng-grid/commit/d0ecd31061c6dc20fa6a558dbc9e696debf83874))
* **column-resizing:** 100% width/height not needed on ui-grid-cell-contents, just ui-grid-inner-cell-c ([6912d14f](http://github.com/angular-ui/ng-grid/commit/6912d14fe4222afe3468df846487b922741ac16b))
* **customizer:** point to ui-grid-unstable.css at top of page ([4892fbc4](http://github.com/angular-ui/ng-grid/commit/4892fbc4804c1bdf39ef6af07b51bcf753ef0849))
* **grunt:**
  * Add testDependencies file array to karma config for travis ([7a15fe42](http://github.com/angular-ui/ng-grid/commit/7a15fe42e85f11962e000fe34a1cb67d6a28ef5e))
  * typo ([0f641c0f](http://github.com/angular-ui/ng-grid/commit/0f641c0ff055f06ea50b54f86e6c686a2d69284b))
  * cut-release wasn't running done() ([025c61b2](http://github.com/angular-ui/ng-grid/commit/025c61b28807d0b6160bbb9d50b12ea42e881695))
  * Make getVersion() work on travis ([129f28b8](http://github.com/angular-ui/ng-grid/commit/129f28b8ae5c6c5678945fbd279c91788ea19a20))
* **less:** header variables were in file twice ([f93f675c](http://github.com/angular-ui/ng-grid/commit/f93f675c2bbb491ebe3fce27ebeb00e9adc5cca8))
* **travis:**
  *  Need only one script command in travis.yml ([2278c691](http://github.com/angular-ui/ng-grid/commit/2278c69122b9bd1b8aaf2225160e015b4a161fab))
  * had grunt args out of order ([c790245a](http://github.com/angular-ui/ng-grid/commit/c790245ace77b6856d4c046f3a08d4ded188648d))
  * use SauceLabs for e2e tests on travis ([5d8a21da](http://github.com/angular-ui/ng-grid/commit/5d8a21da57a466541aef5b16b032a21e46bbf15b))
  * accidentally switched a couple words ([d67c1871](http://github.com/angular-ui/ng-grid/commit/d67c1871a748d036cc386acc559a7dcfd80a32c3))
* **ui-grid-body:**
  * normalize mouse wheel events because browsers suck ([2075bf3c](http://github.com/angular-ui/ng-grid/commit/2075bf3c04e68b0bbedb378f6d16e7cfe5b4e149))
  * scrolling with mousewheel works ([7933c1a4](http://github.com/angular-ui/ng-grid/commit/7933c1a4f353aaebe5ecb2e9c06fd472564b8152))
* **uiGrid:**
  * introduced a bug that added columns over and over when data was swapped ([7fc55b5d](http://github.com/angular-ui/ng-grid/commit/7fc55b5d255b5940139717e35e891a7b3a5bee3a))
  * Fix elm height calc for hidden grids ([420a0dcf](http://github.com/angular-ui/ng-grid/commit/420a0dcf00351d3b5c256f38da1bc2796d869ddf))
* **uiGridCell:** re-compile if template changes ([7485e6ef](http://github.com/angular-ui/ng-grid/commit/7485e6efc8ee8105b3bb835e7f82d7f1997dcac6))
* **uiGridHeader:**
  * handle leftover pixels ([64941b3a](http://github.com/angular-ui/ng-grid/commit/64941b3a82b5ae72b1c75b1e46a6dfed2d68a696))
  * Fix header height calculation ([b1562854](http://github.com/angular-ui/ng-grid/commit/b1562854c808814695ab436b2ba63cd1afa28d4d))
* **uiGridStyle:** <br>s were being needlessly added ([ade12ec9](http://github.com/angular-ui/ng-grid/commit/ade12ec98f82400ddbc25ac2676fc88e66408164))


#### Features

* **AutoResize:** Adding auto-resize feature. ([d0bdd829](http://github.com/angular-ui/ng-grid/commit/d0bdd8295121a9b58a8a3f8ca3a52cdcba400fc0))
* **RTL:** Adding RTL support and fixing virtualization ([d5a9982d](http://github.com/angular-ui/ng-grid/commit/d5a9982d1f6562ad4a379cfcf53165247065fe3f))
* **Selection:** Updates ([b20c41c1](http://github.com/angular-ui/ng-grid/commit/b20c41c1715d7c5745bee7c3ea05ec585ab9bd16))
* **e2e:** protractor testing of docs now works properly with 'grunt dev' ([08787be4](http://github.com/angular-ui/ng-grid/commit/08787be454890a3fbaae324c6741670a59a50f3f))
* **filtering:** Add row filtering ([16161643](http://github.com/angular-ui/ng-grid/commit/16161643fa4eb925e816252937fadfabc59f55a0))
* **row-hashing:** Add new row hashing feature ([da87ff9a](http://github.com/angular-ui/ng-grid/commit/da87ff9a4082c0f9f8996d4bdb5225ce191392ef))
* **rowSort:** Added row sorting ([ce330978](http://github.com/angular-ui/ng-grid/commit/ce330978fe566e9695c30acef04e55221e520960))
* **uiGridStyle:** Added ui-grid-style directive ([5687723f](http://github.com/angular-ui/ng-grid/commit/5687723f318ac322fa84ce96cc9b7cb97e2946f2))

<a name="2.0.7"></a>
## 2.0.7 *(2013-07-01)*

### Features
This release is mainly focused on fixing the grid layout/column widths:

Columns
----------------------------------

*  Removed border-left and border-right from columns. Now we are using vertical bars so if someone sets a width to be 400px for a column, the column will actually be 400px, not 401-402px due to the border. This caused the horizontal overflowing to happen producing a horizontal scrollbar. This also fixed issues like https://github.com/angular-ui/ng-grid/issues/411 where you would see columns not extend all the way to the edge and you would get double borders
* Percent calculation is handled before asterisks calculations because percent calculation should take higher priority, and the asterisks calculations will then be able to fill the remaining space instead of horizontally overflowing the viewport


----------------------------------
A fix contributed by @swalters for #436:

Editing Cells
----------------------------------

When editing a cell, the ng-cell-has-focus directive will broadcast a message named ngGridEventStartCellEdit to let all children know that you can now give yourself focus. When the editable cell template is done with editing (usually on a blur event) you need to emit ngGridEventEndCellEdit to let ng-cell-has-focus know that you are done editing and it will then show the non-editable cell template. The reasoning for this is (good quote): "Now I can wrap my input elements in divs/spans, whatever and control exactly what element's blur triggers the end edit" - @swalters. An example (used for ng-input directive):

scope.$on('ngGridEventStartCellEdit', function () {
    elm.focus();
 });

angular.element(elm).bind('blur', function () {
    scope.$emit('ngGridEventEndCellEdit');
});

Also, there is another option now which is enableCellEditOnFocus (yes, it's coming back) so now you can choose between excel-like editing or enabling edit mode on focus.

----------------------------------
Also some fixes contributed by @ebbe-brandstrup are:

configureColumnWidths
----------------------------------
* Columns sized with * or % now expand / shrink as other * or %-based columns are hidden / shown
  * Note: the changes auto-expand/shrink only take effect on-the-fly
  * Works with grouping and when enabling the selection checkbox row (showSelectionCheckbox)
* Bugfixes in configureColumnWidths
  * Re-ordered columns now keep their width setup
  * Fixed "asteriskNum" so it no longer includes hidden columns (was checking .visible on a columnDefs column instead of the matching ngColumn)
  * Fixed "totalWidth" so it no longer includes hidden columns when using px values for width (was checking .visible on a columnDefs column instead of the matching ngColumn)
  * Fixed ngColumn width being initialized to undefined when using "auto" for width, regardless of "minWidth" settings (was checking .minWidth on a columnDefs column instead of the matching ngColumn)

Renamed "col" to "colDef" in configureColumnWidths() in the places where "col" was a column from "columnDefs". It made it clearer for me whether I was referring to a ngColumn or a column from columnDefs. There were a couple of bugs caused by that (col.visible incorrectly accessed on columnDefs objects instead of ngColumns, and the like).

ng-grid-flexible-height plugin
----------------------------------------
* Bugfixes in ng-grid-flexible-height
  * The plugin couldn't shrink the grid, only grow it
  * Using domUtilityService.UpdateGridLayout instead of grid.refreshDomSizes which correctly grows the grid if it's been shrunk (e.g. when paging to the last page and it has few rows + the plugin has a smaller min. height than what's needed on the other pages)

### Bug fixes
Too many to list. Here is the pull request https://github.com/angular-ui/ng-grid/pull/511


<a name="2.0.6"></a>
## 2.0.6 *(2013-06-?)*

### Features

- **Development**
  - Continuous integration testing with [Travis CI](https://travis-ci.org/angular-ui/ng-grid). A few tests that were looking for pixel perfection had to be relaxed due to rendering differences between browsers and OSes.
  - Moved this changelog to CHANGELOG.md!
  - Added tests for i18n languages. Any new language must cover all the properties that the default language (English) has.
  - CSS files compiling with less ([24bb173](https://github.com/angular-ui/ng-grid/commit/24bb173))
  - Added optional --browsers flag for test tasks. `grunt test --browsers=Chrome,Firefox,PhantomJS` will test in all 3 browsers at once.

- **Sorting**
  - Allow optional '+' prefix to trigger numerical sort ([f3aff74](https://github.com/angular-ui/ng-grid/commit/f3aff74), [8e5c0a1](https://github.com/angular-ui/ng-grid/commit/8e5c0a1))
  - Standardizing sort arrow direction (thanks @dcolens) ([9608488](https://github.com/angular-ui/ng-grid/commit/9608488))

- **i18n**
  - Added Brazilian Portugeuse (thanks @dipold) ([ab0f207](https://github.com/angular-ui/ng-grid/commit/ab0f207))

### Bug fixes

- Allow column `displayName` to be an empty string. ([#363](https://github.com/angular-ui/ng-grid/issues/363), [46a992f](https://github.com/angular-ui/ng-grid/commit/46a992f))
- Fix for `totalServerItems` not updating ([#332](https://github.com/angular-ui/ng-grid/issues/332), [#369](https://github.com/angular-ui/ng-grid/issues/369), [fcfe316](https://github.com/angular-ui/ng-grid/commit/fcfe316))
- Fixed regression in [2.0.5](#2.0.5) that used Array.forEach, which isn't supported in IE8. Moved to angular.forEach ([e4b08a7](https://github.com/angular-ui/ng-grid/commit/e4b08a7))
- Fixed and added tests for wysiwyg-export plugin ([57df36f](https://github.com/angular-ui/ng-grid/commit/57df36f))
- Fixed extraneous trailing comma ([#449](https://github.com/angular-ui/ng-grid/issues/449), [2c655c7](https://github.com/angular-ui/ng-grid/commit/2c655c7))
- **Cell editing** - various attempts at fixing broken cell editing eventually resulted in using [NgModelController](http://docs.angularjs.org/api/ng.directive:ngModel.NgModelController) (thanks @swalters). ([#442](https://github.com/angular-ui/ng-grid/issues/442), [050a1ba](https://github.com/angular-ui/ng-grid/commit/050a1ba), [5c82f9b](https://github.com/angular-ui/ng-grid/commit/5c82f9b), [5c82f9b](https://github.com/angular-ui/ng-grid/commit/5c82f9b), [f244363](https://github.com/angular-ui/ng-grid/commit/f244363), [ee2a5f1](https://github.com/angular-ui/ng-grid/commit/ee2a5f1))

<a name="2.0.5"></a>
## 2.0.5 *(2013-04-23)*

### Features

- Moving to $http for external template fetching. Should fix issues with grid rendering before templates are retrieved, as well as fetching the same template multiple times.

### Bug fixes

- Fixed bug that prevented the grid from maintaining row selections post-sort thanks to [sum4me](https://github.com/sum4me)

<a name="2.0.4"></a>
## 2.0.4 *(2013-04-08)*

### Bug fixes

- Fixing some more minor bugs

<a name="2.0.3"></a>
## 2.0.3 *(2013-03-29)*

- Changed default multiSelect behavior, updating some plugins and making some more minor bugfixes.

<a name="2.0.2"></a>
## 2.0.2 *(2013-03-08)*

- minor bugfixes, updating some plugins.

<a name="2.0.1"></a>
## 2.0.1 *(2013-03-05)*

 - Moved to grunt build system. No more international version; all languages are included by default. Fixed minor grouping display issue. Using $templateCache for templates instead of global namespace.

<a name="2.0.0"></a>
## 2.0.0 *(2013-03-05)*

 - Breaking Changes: see documentation (showSelectionBox, enableRowSelection, showFooter). Column Virtualization added. Row virtualization performance improved. Excel-like editing instead of enableFocusedCellEdit.

<a name="1.9.0"></a>
## 1.9.0 *(2013-02-18)*

 - Aggregates now display correctly. Added more option methods to select and group data (see wiki), Added column pinning.

<a name="1.8.0"></a>
## 1.8.0 [Hotfix] *(2013-02-11)*

 - Fixes for multi-level grouping and adding the gridId to both the grid options and as argument to the "ngGridEventData" so you can identify what grid it came from.

<a name="1.8.0"></a>
## 1.8.0 *(2013-02-07)*

 - Major architectural changes which greatly improves performance. virtualizationThreshold now controlls when virtualization is force-enabled and is user-specified in options.

<a name="1.7.1"></a>
## 1.7.1 *(2013-02-06)*

 - Fixed bug with selections and multiple grids. New emit message for notifying when hitting bottom of viewport. Can disable virtualization. ng-grid virtualization is on by default, but can be disabled if there are less than 50 rows in the grid. Anything > 50 rows virtualization is forced on for performance considerations.

<a name="1.7.0"></a>
## 1.7.0 *(2013-02-05)*

 - BREAKING CHANGES: Will add examples. Adding cell selection, navigation, and edit on focus. Added programmatic selections. Improved scrolling. ngGridEvents changed/added: see wiki.

<a name="1.6.3"></a>
## 1.6.3 *(2013-01-17)*

 - Can now highlight/copy text in grid. Fixed multiple issues when using multiselect along with shift key. Refactored key events so now they are all in the same directive for viewport. Hovering over highlightable text will change cursors in viewport. Fixed #93.

<a name="1.6.2"></a>
## 1.6.2 *(2013-01-09)*

 - Merged changes to have two-way data-binding work in templates, so if you're using a celltemplate, you can now use COL_FIELD instead of row.getProperty(col.field). row.getProperty is still in the row class for accessing other row values.

<a name="1.6.1"></a>
## 1.6.1 *(2013-01-08)*

 - Adding ability to preselect rows. Can deselect when multiSelect:false. Bug fixes/merging pull requests. Bower now works. Can now sync external search with ng-grid internal search. Check out other examples on examples page.

<a name="1.6.0"></a>
## 1.6.0 *(2012-12-27)*

 - Adding i18n support and support for different angularjs interpolation symbols (requires building from source).

<a name="1.5.0"></a>
## 1.5.0 *(2012-12-20)*

 - Modifying the way we watch for array changes. Added groupable column definition option. Bugfixes for #58, #59.

<a name="1.4.1"></a>
## 1.4.1 *(2012-12-18)*

 - jslint reformat, minor bugfixes, performance improvements while keydown navigating, adding "use strict" to script.

<a name="1.4.0"></a>
## 1.4.0 *(2012-12-12)*

 - Massive improvements to search thanks to [iNeedFat](https://github.com/ineedfat)!

<a name="1.3.9"></a>
## 1.3.9 *(2012-12-12)*

 - Refactored and removed unneeded code. Added scope events.

<a name="1.3.7"></a>
## 1.3.7 *(2012-12-12)*

 - Improving template compilation and fixing jquery theme support. Improving comments on grid options.

<a name="1.3.6"></a>
## 1.3.6 *(2012-12-06)*

 - sortInfo can now be set to default sort the grid. Improvements to the beforeSelectionChange callback mechanism when multi-selecting.

<a name="1.3.5"></a>
## 1.3.5 *(2012-12-06)*

 - Improved template rendering when using external template files. columnDefs can now be a $scope object which can be push/pop/spliced. Fixed box model for cells and header cells.

<a name="1.3.4"></a>
## 1.3.4 *(2012-12-04)*

 - Improved aggregate grouping, minor bugfixes. Auto-width works!

<a name="1.3.2"></a>
## 1.3.2 *(2012-11-27)*

 - Changed default width behavior to use *s and added option to maintain column ratios while resizing

<a name="1.3.1"></a>
## 1.3.1 *(2012-11-27)*

 - Added layout plugin. Support for uri templates. Performance improvements.

<a name="1.3.0"></a>
## 1.3.0 *(2012-11-23)*

 - Major code refactoring, can now group-by using column menu, changes to build

<a name="1.2.2"></a>
## 1.2.2 *(2012-11-21)*

 - Built-in filtering support, numerous perfomance enhancements and minor code refactoring

<a name="1.2.1"></a>
## 1.2.1 *(2012-11-20)*

 - Added ability to specify property "paths" as fields and for grid options.

<a name="1.2.0"></a>
## 1.2.0 *(2012-11-19)*

 - Added Server-Side Paging support and minor bug fixes.

<a name="1.1.0"></a>
## 1.1.0 *(2012-11-17)*

 - Added ability to hide/show columns and various bug fixes/performance enhancements.

<a name="1.0.0"></a>
## 1.0.0 *(2012-11-14)*

 - Release
