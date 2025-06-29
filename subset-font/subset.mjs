import { subsetIconfont, FaFreeProvider } from 'subset-iconfont';
import { readFileSync, existsSync } from 'fs';

// Configuration des options de subset
const subsetOptions = { 
  formats: ['woff2'],
  fontName: 'GeneWeb 7.1 webfont subset from Font-Awesome v6.7.2',
  fontFileName: 'fa-gw-400',
  prefix: 'fa',
  cssChoices: ['sizing', 'fixed-width', 'list', 'rotated', 'flipped', 'stacked']
};

/**
 * Fonction pour charger la liste des icônes depuis subset-icons.txt
 * Si le fichier n'existe pas, utilise la liste codée en dur comme fallback
 */
function loadIconsList() {
  const iconsFile = 'subset-icons.txt';
  
  // Vérifier si le fichier subset-icons.txt existe
  if (existsSync(iconsFile)) {
    try {
      console.log('📖 Lecture de la liste d\'icônes depuis subset-icons.txt...');
      
      // Lire le contenu du fichier
      const fileContent = readFileSync(iconsFile, 'utf8').trim();
      
      // Parser la liste d'icônes
      const iconMatches = fileContent.match(/'([^']+)'/g);
      
      if (iconMatches && iconMatches.length > 0) {
        const iconsList = iconMatches.map(match => match.replace(/'/g, ''));
        console.log(`✅ ${iconsList.length} icônes chargées depuis ${iconsFile}`);
        
        const sampleIcons = iconsList.slice(0, 5).join(', ');
        console.log(`📋 Échantillon: ${sampleIcons}${iconsList.length > 5 ? '...' : ''}`);
        
        return iconsList;
      } else {
        console.warn('⚠️  Format invalide dans subset-icons.txt, utilisation de la liste de fallback');
        return getFallbackIconsList();
      }
      
    } catch (error) {
      console.error('❌ Erreur lors de la lecture de subset-icons.txt:', error.message);
      console.log('🔄 Utilisation de la liste de fallback...');
      return getFallbackIconsList();
    }
  } else {
    console.log('📄 subset-icons.txt non trouvé, utilisation de la liste de fallback');
    console.log('💡 Exécutez ./extract-geneweb-subset.sh pour générer subset-icons.txt');
    return getFallbackIconsList();
  }
}

/**
 * Liste d'icônes de fallback 20250629
 */
function getFallbackIconsList() {
  return [
    'a', 'address-book', 'address-card', 'align-center', 'align-justify', 'align-left', 'angle-left', 'angle-right', 'arrow-down', 'arrow-down-1-9', 'arrow-down-9-1', 'arrow-down-a-z', 'arrow-down-long', 'arrow-down-wide-short', 'arrow-down-z-a', 'arrow-left', 'arrow-left-long', 'arrow-right', 'arrow-right-arrow-left', 'arrow-rotate-left', 'arrow-rotate-right', 'arrow-turn-up', 'arrow-up', 'arrow-up-from-bracket', 'arrow-up-long', 'arrow-up-right-dots', 'arrows-spin', 'award', 'baby', 'backward', 'bezier-curve', 'bold', 'book', 'border-none', 'box-archive', 'bug', 'cake-candles', 'calendar-days', 'caret-left', 'caret-right', 'chart-bar', 'chart-pie', 'check', 'chess-board', 'chess-rook', 'chevron-down', 'chevron-left', 'chevron-right', 'chevron-up', 'child', 'children', 'circle-dot', 'circle-info', 'circle-question', 'circle-stop', 'circle-xmark', 'clipboard', 'clock-rotate-left', 'closed-captioning', 'code-branch', 'code-fork', 'comment', 'comment-dots', 'comment-slash', 'comments', 'compress', 'crop', 'crop-simple', 'cross', 'crown', 'desktop', 'diagram-project', 'dice', 'dice-five', 'dice-four', 'dice-one', 'dice-six', 'dice-three', 'dice-two', 'dna', 'dove', 'down-long', 'earth-americas', 'elevator', 'ellipsis', 'equals', 'file', 'file-csv', 'file-image', 'file-lines', 'file-pdf', 'file-zipper', 'filter', 'folder', 'folder-open', 'forward', 'gear', 'github', 'globe', 'graduation-cap', 'hat-wizard', 'heading', 'highlighter', 'hourglass-half', 'house', 'id-card', 'image', 'image-portrait', 'images', 'indent', 'info', 'italic', 'less-than-equal', 'lightbulb', 'link', 'link-slash', 'list', 'list-ul', 'magnifying-glass', 'magnifying-glass-minus', 'magnifying-glass-plus', 'map-location-dot', 'markdown', 'mars', 'mars-double', 'mask', 'minus', 'neuter', 'newspaper', 'note-sticky', 'paintbrush', 'pen', 'pen-to-square', 'pencil', 'people-arrows', 'people-group', 'percent', 'person', 'person-arrow-down-to-line', 'person-arrow-up-from-line', 'person-breastfeeding', 'person-circle-check', 'person-circle-minus', 'person-circle-plus', 'person-circle-question', 'person-dress', 'person-praying', 'plus', 'power-off', 'question', 'restroom', 'retweet', 'right-from-bracket', 'right-left', 'ring', 'rotate', 'screwdriver-wrench', 'share', 'share-nodes', 'shoe-prints', 'shuffle', 'signature', 'sitemap', 'skull-crossbones', 'sort', 'sort-down', 'sort-up', 'spell-check', 'star', 'strikethrough', 'superscript', 'table', 'table-cells', 'thumbs-down', 'thumbs-up', 'timeline', 'toggle-off', 'toggle-on', 'trash', 'trash-can', 'triangle-exclamation', 'turn-up', 'underline', 'up-long', 'user', 'user-doctor', 'user-gear', 'user-group', 'user-large', 'user-pen', 'user-plus', 'user-shield', 'user-tie', 'user-xmark', 'users', 'venus', 'venus-double', 'venus-mars', 'wand-magic-sparkles', 'wikipedia-w', 'window-minimize', 'wrench', 'xmark', 'z'
  ];
}

// Charger la liste des icônes
const iconsList = loadIconsList();

// Créer le provider Font Awesome avec la liste chargée
console.log('🚀 Création du subset Font Awesome...');
const fa = new FaFreeProvider(iconsList);

// Lancer la génération du subset
subsetIconfont([fa], './gw-font-subset', subsetOptions).then((result) => {
  console.log('✅ Font Awesome webfonts subset for Geneweb is generated in "gw-font-subset" subdir.');
  console.log(`📊 Résumé: ${iconsList.length} icônes traitées`);
}).catch((error) => {
  console.error('❌ Erreur lors de la génération:', error);
  process.exit(1);
});
