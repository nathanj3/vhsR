# vhsr_doctor reports all-missing state cleanly

    Code
      out <- vhsr_doctor()
    Message
      
      -- vhsR diagnostic --
      
      x vhs: not found
      x ttyd: not found
      x ffmpeg: not found
      
      -- Optional (syntax highlighting) 
      ! arf: not found (<https://github.com/eitsupi/arf> - modern Rust-based R console, preferred backend)
      ! radian: not found (`pip install radian` - fallback when arf is absent)
      ! colorout: not installed (`vhsr_install_colorout()` for output coloring under plain R)
      i Run `vhsr_install()` to install missing required binaries.

# vhsr_doctor reports a fully-equipped system

    Code
      out <- vhsr_doctor()
    Message
      
      -- vhsR diagnostic --
      
      v vhs v1.0.0 (system): '<cache>/vhs'
      v ttyd v1.0.0 (system): '/system/bin/ttyd'
      v ffmpeg v1.0.0 (system): '/system/bin/ffmpeg'
      
      -- Optional (syntax highlighting) 
      v arf v1.0.0 (system): '/system/bin/arf'
      v radian v1.0.0 (system): '/system/bin/radian'
      v colorout: installed (output coloring under plain R)

