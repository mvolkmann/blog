import Prism from 'prismjs';

import 'prismjs/components/prism-bash';
import 'prismjs/components/prism-c';
import 'prismjs/components/prism-cpp';
import 'prismjs/components/prism-csharp';
import 'prismjs/components/prism-css';
import 'prismjs/components/prism-dart';
import 'prismjs/components/prism-diff';
import 'prismjs/components/prism-docker';
import 'prismjs/components/prism-go';
import 'prismjs/components/prism-groovy';
import 'prismjs/components/prism-java';
import 'prismjs/components/prism-json';
import 'prismjs/components/prism-jsx';
import 'prismjs/components/prism-kotlin';
import 'prismjs/components/prism-latex';
import 'prismjs/components/prism-markdown';
import 'prismjs/components/prism-markup-templating';
import 'prismjs/components/prism-nginx';
import 'prismjs/components/prism-nix';
import 'prismjs/components/prism-php';
import 'prismjs/components/prism-powershell';
import 'prismjs/components/prism-python';
import 'prismjs/components/prism-regex';
import 'prismjs/components/prism-ruby';
import 'prismjs/components/prism-rust';
import 'prismjs/components/prism-sass';
import 'prismjs/components/prism-scala';
import 'prismjs/components/prism-scss';
import 'prismjs/components/prism-sql';
import 'prismjs/components/prism-swift';
import 'prismjs/components/prism-tsx';
import 'prismjs/components/prism-typescript';
import 'prismjs/components/prism-yaml';

// Some posts use shell aliases that Prism classifies under plain "shell".
Prism.languages.shell = Prism.languages.bash;
Prism.languages.zsh = Prism.languages.bash;

Prism.highlightAll();
