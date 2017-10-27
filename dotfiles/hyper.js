// Future versions of Hyper may add additional config options,
// which will not automatically be merged into this file.
// See https://hyper.is#cfg for all currently supported options.

module.exports = {
    config: {
        fontSize: 12,
        fontFamily: 'Menlo, "DejaVu Sans Mono", Consolas, "Lucida Console", monospace',
        cursorColor: 'rgba(248,28,229,0.8)',
        cursorShape: 'BLOCK',
        cursorBlink: false,
        padding: '12px 14px',
        shell: '',
        shellArgs: ['--login'],
        env: {},
        bell: false,
        copyOnSelect: true
    },
    
    plugins: [
        'nord-hyper',
    ],
};
