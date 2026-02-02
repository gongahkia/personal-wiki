document.addEventListener('DOMContentLoaded', () => {
    const output = document.getElementById('output');
    const commandInput = document.getElementById('command-input');
    const terminal = document.getElementById('terminal');

    const content = {
        'whoami': 'Gabriel Ong (王哲勉)<br>' +
                  'Full Stack Developer specialising in Legal Technology.<br>' +
                  'Singapore (GMT+8)',

        'contact': '<span class="section-title">Accounts</span>' +
                   '<dl>' +
                   '<dt>LinkedIn</dt><dd><a href="https://www.linkedin.com/in/gabriel-zmong/">https://www.linkedin.com/in/gabriel-zmong/</a></dd>' +
                   '<dt>GitHub</dt><dd><a href="https://github.com/gongahkia">https://github.com/gongahkia</a></dd>' +
                   '<dt>X (Twitter)</dt><dd><a href="https://x.com/gongahkia">https://x.com/gongahkia</a></dd>' +
                   '<dt>Letterboxd</dt><dd><a href="https://letterboxd.com/gongtalksfilm/">https://letterboxd.com/gongtalksfilm/</a></dd>' +
                   '</dl>' +
                   '<span class="section-title">Writing</span>' +
                   '<dl>' +
                   '<dt>Blog</dt><dd><a href="https://gabrielongzm.com/blog/">https://gabrielongzm.com/blog/</a></dd>' +
                   '<dt>Personal Wiki</dt><dd><a href="https://gabrielongzm.com/personal-wiki/">https://gabrielongzm.com/personal-wiki/</a></dd>' +
                   '</dl>' +
                   '<span class="section-title">Resume</span>' +
                   '<dl>' +
                   '<dt>PDF</dt><dd><a href="./resume/main.pdf">main.pdf</a></dd>' +
                   '</dl>' +
                   '<span class="section-title">Mail</span>' +
                   '<dl>' +
                   '<dt>Business</dt><dd><a href="mailto:gabrielzmong@gmail.com">gabrielzmong@gmail.com</a></dd>' +
                   '<dt>School</dt><dd><a href="mailto:gabriel.ong.2023@scis.smu.edu.sg">gabriel.ong.2023@scis.smu.edu.sg</a></dd>' +
                   '</dl>',

        'works': '<span class="section-title">Works</span>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/yuho">yuho</a></h3><p>Domain-specific language for Singapore Criminal Law</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/sea-kayak">sea-kayak</a></h3><p>Singapore\'s Daily Legal Updates Web App</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/skill-hunter">skill-hunter</a></h3><p>SSO Legislation Browser Extension</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/jikai">jikai</a></h3><p>Tort Law hypothetical Generation Model</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/junas">junas</a></h3><p>A actually helpful Legal Agent Web App</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia/dc4u">dc4u</a></h3><p>Markup language for Charge Sheet drafting</p></div>' +
                 '<div class="command-output"><h3><a href="https://github.com/gongahkia">GitHub Contributions</a></h3><p>A graph of my contributions over the past year.</p></div>',

        'skills': '<span class="section-title">Skills</span>' +
                  '<dl>' +
                  '<dt>Frontend</dt><dd>React, React Native, Next.js, Vue.js, Electron.js, Svelte, Flutter, Quasar, Cordova</dd>' +
                  '<dt>Backend</dt><dd>Python, JavaScript, TypeScript, Java, Go, Rust, SQL, Node.js, PHP, Dart, Directus, Hookdeck</dd>' +
                  '<dt>Cloud</dt><dd>GCP, Supabase, Firebase, MongoDB, ChromaDB, Pinecone, AWS, Heroku, Cloudflare, Docker, Kubernetes, CI/CD</dd>' +
                  '<dt>Others</dt><dd>Linux, Vulkan, WebGL, WebGPU, WebLLM, TensorFlow, PyTorch, Hugging Face, Pandas, Haskell, Lua</dd>' +
                  '<dt>Legal</dt><dd>Legal writing and research, Contract Law, Criminal Law, Tort Law, Intellectual Property Law, Privacy Law, Company Law</dd>' +
                  '<dt>Languages</dt><dd>English (native), Mandarin (bilingual), Japanese (elementary), Korean (beginner)</dd>' +
                  '</dl>',

        'education': '<span class="section-title">Education</span>' +
                     '<dl>' +
                     '<dt>Aug 2023 - Apr 2027</dt><dd>Bachelor of Science (Computing and Law)<br>Singapore Management University<br>GPA: 3.60/4.00</dd>' +
                     '<dt>Jan 2019 - Dec 2020</dt><dd>International Baccalaureate Diploma Programme (IBDP)<br>Anglo Chinese School (Independent)<br>Score: 44/45</dd>' +
                     '</dl>',

        'experience': '<span class="section-title">Experience</span>' +
                      '<dl>' +
                      '<dt>Jan 2026 - May 2026</dt><dd>Elefant - Full Stack Development Intern</dd>' +
                      '<dt>Aug 2025 - Dec 2025</dt><dd>WhyHow.AI - Software Engineer Intern</dd>' +
                      '<dt>May 2025 - Aug 2025</dt><dd>SMU Yong Pung How School of Law - Research Assistant</dd>' +
                      '<dt>May 2025 - Aug 2025</dt><dd>Custom Automated Systems - Full Stack Development Intern</dd>' +
                      '<dt>Jan 2025 - Apr 2025</dt><dd>SMU Yong Pung How School of Law - Research Assistant</dd>' +
                      '<dt>Aug 2024 - Mar 2025</dt><dd>The Modo Collective - Founder, Lead Developer</dd>' +
                      '<dt>Sep 2024 - Dec 2024</dt><dd>Elefant - Backend Development Intern</dd>' +
                      '<dt>Aug 2023 - Oct 2024</dt><dd>SMU Legal Innovation and Technology (LIT) - Tech Development Director</dd>' +
                      '<dt>Apr 2024 - Aug 2024</dt><dd>The Yap Labs - Co-Founder, Tech Lead</dd>' +
                      '</dl>',

        'awards': '<span class="section-title">Awards</span>' +
                  '<dl>' +
                  '<dt>Sep 2025</dt><dd>Champion of SMU LIT Hackathon 2025 - MinLaw Track 2</dd>' +
                  '<dt>Aug 2025</dt><dd>Second place for Vibe-Building Hackathon 2025</dd>' +
                  '<dt>Feb 2025</dt><dd>GeeksHacking Choice Award for HackOMania 2025</dd>' +
                  '<dt>Sep 2024</dt><dd>Champion of YouthxHack 2024 - Digital Defence</dd>' +
                  '<dt>Aug 2023 - Apr 2027</dt><dd>Global Impact Scholarship - Singapore Management University</dd>' +
                  '</dl>',

        'certifications': '<span class="section-title">Licenses & Certifications</span>' +
                          '<dl>' +
                          '<dt>Feb 2025</dt><dd>Data Manipulation with pandas</dd>' +
                          '<dt>Jul 2024</dt><dd>Google Cybersecurity Specialization</dd>' +
                          '<dt>Jun 2024</dt><dd>Google Advanced Data Analytics Specialization</dd>' +
                          '<dt>May 2024</dt><dd>Google Digital Marketing & E-commerce Specialization</dd>' +
                          '<dt>Dec 2023</dt><dd>Building Web Applications in PHP</dd>' +
                          '<dt>Jun 2021</dt><dd>Advanced Certificate in Infocomm Technology</dd>' +
                          '</dl>',

        'help': 'Available commands:<br>' +
                '  whoami          - Display my information<br>' +
                '  contact         - Show my accounts, resume, and mail<br>' +
                '  works           - List my projects<br>' +
                '  skills          - Show my technical skills<br>' +
                '  education       - List my education history<br>' +
                '  experience      - List my work experience<br>' +
                '  awards          - List my awards<br>' +
                '  certifications  - List my licenses and certifications<br>' +
                '  clear           - Clear the terminal<br>' +
                '  help            - Show this help message'
    };

    function type(text, i = 0) {
        if (i < text.length) {
            if (text.substring(i, i + 4) === '<br>') {
                output.innerHTML += '<br>';
                i += 4;
            } else {
                output.innerHTML += text.charAt(i);
                i++;
            }
            setTimeout(() => type(text, i), 50);
        }
    }

    function executeCommand(command) {
        output.innerHTML += `<div class="prompt-line"><span class="prompt">guest@gabrielong.com:~$</span> ${command}</div>`;
        if (command === 'clear') {
            output.innerHTML = '';
        } else if (content[command]) {
            output.innerHTML += `<div class="command-output">${content[command]}</div>`;
        } else {
            output.innerHTML += `<div>-bash: ${command}: command not found</div>`;
        }
        terminal.scrollTop = terminal.scrollHeight;
    }

    commandInput.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') {
            const command = commandInput.value.trim();
            if (command) {
                executeCommand(command);
                commandInput.value = '';
            }
        }
    });

    type("Welcome to my portfolio. Type 'help' to see available commands.<br>© 2023-2026 Gabriel Ong. All rights reserved.<br><br>");
});

