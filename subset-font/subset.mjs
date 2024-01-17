import { subsetIconfont, FaFreeProvider } from 'subset-iconfont';

const subsetOptions = { 
formats: ['woff2'],
fontName: 'GeneWeb 7.1 webfont subset from Font-Awesome v6.5.1',
fontFileName: 'fa-gw-400',
prefix: 'fa',
cssChoices: ['sizing', 'fixed-width', 'list', 'rotated', 'flipped', 'stacked']
};

const fa = new FaFreeProvider([
'a', 'address-book', 'address-card', 'align-center', 'align-justify', 'align-left', 'angle-left', 'angle-right', 'arrow-down-1-9', 'arrow-down-9-1', 'arrow-down-a-z', 'arrow-down-long', 'arrow-down-wide-short', 'arrow-down-z-a', 'arrow-left', 'arrow-left-long', 'arrow-right', 'arrow-right-arrow-left', 'arrow-rotate-left', 'arrow-rotate-right', 'arrow-turn-up', 'arrow-up', 'arrow-up-long', 'arrow-up-right-dots', 'award', 'baby', 'backward', 'bezier-curve', 'bold', 'book', 'border-none', 'box-archive', 'bug', 'cake-candles', 'calendar-days', 'caret-left', 'caret-right', 'chart-bar', 'chart-pie', 'check', 'chess-board', 'chevron-down', 'chevron-left', 'chevron-right', 'chevron-up', 'child', 'circle-dot', 'circle-stop', 'clipboard', 'clock-rotate-left', 'closed-captioning', 'code-branch', 'code-fork', 'comment-dots', 'comments', 'comment-slash', 'compress', 'crop', 'crop-simple', 'cross', 'crown', 'desktop', 'diagram-project', 'dice', 'dice-five', 'dice-four', 'dice-one', 'dice-six', 'dice-three', 'dice-two', 'dna', 'dove', 'down-long', 'earth-americas', 'elevator', 'ellipsis', 'equals', 'file', 'file-csv', 'file-image', 'file-lines', 'file-pdf', 'file-zipper', 'filter', 'folder-open', 'forward', 'gear', 'github', 'globe', 'graduation-cap', 'hat-wizard', 'heading', 'highlighter', 'hourglass-half', 'house', 'id-card', 'image', 'image-portrait', 'images', 'indent', 'info', 'italic', 'less-than-equal', 'link', 'link-slash', 'list', 'list-ul', 'magnifying-glass', 'magnifying-glass-minus', 'magnifying-glass-plus', 'markdown', 'mars', 'mars-double', 'minus', 'neuter', 'newspaper', 'note-sticky', 'paintbrush', 'pen-to-square', 'people-arrows', 'percent', 'person', 'person-arrow-down-to-line', 'person-arrow-up-from-line', 'person-dress', 'person-praying', 'plus', 'power-off', 'question', 'restroom', 'retweet', 'right-from-bracket', 'right-left', 'ring', 'rotate', 'screwdriver-wrench', 'share', 'share-nodes', 'shoe-prints', 'shuffle', 'sitemap', 'skull-crossbones', 'sort', 'sort-down', 'sort-up', 'star', 'superscript', 'table', 'table-cells', 'thumbs-down', 'thumbs-up', 'timeline', 'toggle-off', 'toggle-on', 'trash', 'trash-can', 'triangle-exclamation', 'turn-up', 'underline', 'up-long', 'user', 'user-gear', 'user-group', 'user-large', 'user-pen', 'user-plus', 'users', 'user-tie', 'user-xmark', 'venus', 'venus-double', 'venus-mars', 'wand-magic-sparkles', 'wikipedia-w', 'window-minimize', 'wrench', 'xmark', 'z'
]);

subsetIconfont([fa], './gw-font-subset', subsetOptions).then((result) => {
  console.log('Font Awesome webfonts subset for Geneweb is generated in “gw-font-subset” subdir.');
});
