(define-module (systemic packages ghostty)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system emacs)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))


;;;
;;; Ghostel.
;;;

(define-public emacs-ghostel
  (let* ((version "0.42.1")
         (ghostty-version "1.3.2-dev")
         (ghostty-commit "c41c6b81a4642ccba18d47b375d9495664de72a0")
         (uucode-version "0.2.0")
         (ghostty-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ghostty-org/ghostty")
                  (commit ghostty-commit)))
            (file-name (git-file-name "ghostty" ghostty-commit))
            (sha256
             (base32
              "1n95krphcpq9p35mcn45fh8g86kpz93ghy93hzpkynxij4w74528"))))
         (uucode-source
          (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/jacobsandlund/uucode")
                  (commit (string-append "v" uucode-version))))
            (file-name (git-file-name "uucode" uucode-version))
            (sha256
             (base32
              "1a3lrmbpc4ifdj1z6ra2b3xnfwh784q2bx835pz58hwpc2pf3flc")))))
    (package
      (name "emacs-ghostel")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dakra/ghostel")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01hpi8n3w0l6sikl9ygwfl59yvrpc03spwkci85r7yjq6brj9mfl"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:lisp-directory "lisp"
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'unpack-zig-dependencies
              (lambda _
                (let ((deps (string-append (dirname (getcwd)) "/deps")))
                  (mkdir-p deps)
                  (copy-recursively #$ghostty-source
                                    (string-append deps "/ghostty")
                                    #:log (%make-void-port "w"))
                  (copy-recursively #$uucode-source
                                    (string-append deps "/uucode")
                                    #:log (%make-void-port "w"))
                  (for-each make-file-writable
                            (find-files deps #:directories? #t)))))
            (add-after 'unpack-zig-dependencies 'patch-zig-dependencies
              (lambda _
                (substitute* "ghostel.el"
                  (("\\\\\"/bin/bash\\\\\"") "\\\"bash\\\""))
                (substitute* "ghostel.el"
                  (("\\\\\"/bin/zsh\\\\\"") "\\\"zsh\\\""))
                (substitute* "ghostel-debug.el"
                  (("\\\\\"/bin/sh\\\\\"") "\\\"sh\\\""))
                (let ((root (dirname (getcwd))))
                  (with-directory-excursion root
                    (with-output-to-file "build.zig.zon"
                      (lambda ()
                        (format #t "\
.{
    .name = .ghostel,
    .version = \"~a\",
    .paths = .{\"\"},
    .fingerprint = 0x5a44bdd1198a0f4b,
    .dependencies = .{
        .ghostty = .{ .path = \"./deps/ghostty\" },
    },
}
"
                                #$version)))
                    (with-output-to-file "deps/ghostty/build.zig.zon"
                      (lambda ()
                        (format #t "\
.{
    .name = .ghostty,
    .version = \"~a\",
    .paths = .{\"\"},
    .fingerprint = 0x64407a2a0b4147e5,
    .minimum_zig_version = \"0.15.2\",
    .dependencies = .{
        .android_ndk = .{ .path = \"./pkg/android-ndk\" },
        .apple_sdk = .{ .path = \"./pkg/apple-sdk\" },
        .uucode = .{ .path = \"../uucode\" },
    },
}
"
                                #$ghostty-version)))

                    ;; Ghostel only needs libghostty-vt.  Ghostty's normal
                    ;; build graph still mentions UI resources, benchmarks,
                    ;; platform packages, and optional SIMD code.  Keep this
                    ;; dependency graph local and small by replacing non-VT
                    ;; helpers with inert stubs and using path dependencies.
                    (substitute* "build.zig"
                      (("\\.@\"emit-lib-vt\" = true,")
                       ".@\"emit-lib-vt\" = true,\n        .simd = false,"))
                    (let ((ghostty-exe-pattern
                           (string-append
                            "    if \\(!cfg\\.emit_lib_vt\\) _ = try "
                            "deps\\.add\\(exe\\);")))
                      (substitute* "deps/ghostty/src/build/GhosttyExe.zig"
                        ((ghostty-exe-pattern) "    _ = deps;")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyBench.zig"
                      (lambda ()
                        (display "\
const GhosttyBench = @This();
const std = @import(\"std\");
const SharedDeps = @import(\"SharedDeps.zig\");

steps: []*std.Build.Step.Compile,

pub fn init(
    b: *std.Build,
    deps: *const SharedDeps,
) !GhosttyBench {
    _ = deps;
    return .{
        .steps = try b.allocator.alloc(
            *std.Build.Step.Compile,
            0,
        ),
    };
}

pub fn install(self: *const GhosttyBench) void {
    _ = self;
}
")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyFrameData.zig"
                      (lambda ()
                        (display "\
const GhosttyFrameData = @This();
const std = @import(\"std\");
const DistResource = @import(\"GhosttyDist.zig\").Resource;

output: std.Build.LazyPath,

pub fn init(b: *std.Build) !GhosttyFrameData {
    const wf = b.addWriteFiles();
    const zig_file = wf.add(
        \"framedata.zig\",
        \"pub const compressed = \\\"\\\";\\n\",
    );
    return .{ .output = zig_file };
}

pub fn addImport(
    self: *const GhosttyFrameData,
    step: *std.Build.Step.Compile,
) void {
    self.output.addStepDependencies(&step.step);
    step.root_module.addAnonymousImport(
        \"framedata\",
        .{ .root_source_file = self.output },
    );
}

pub fn distResources(b: *std.Build) struct {
    framedata: DistResource,
} {
    const file = b.addWriteFiles().add(
        \"framedata.compressed\",
        \"\",
    );
    return .{
        .framedata = .{
            .dist = \"src/build/framegen/framedata.compressed\",
            .generated = file,
        },
    };
}
")))
                    (with-output-to-file
                        "deps/ghostty/src/build/GhosttyResources.zig"
                      (lambda ()
                        (display "\
const GhosttyResources = @This();
const std = @import(\"std\");
const Config = @import(\"Config.zig\");
const SharedDeps = @import(\"SharedDeps.zig\");

steps: []*std.Build.Step,

pub fn init(
    b: *std.Build,
    cfg: *const Config,
    deps: *const SharedDeps,
) !GhosttyResources {
    _ = cfg;
    _ = deps;
    return .{
        .steps = try b.allocator.alloc(
            *std.Build.Step,
            0,
        ),
    };
}

pub fn install(self: *const GhosttyResources) void {
    _ = self;
}

pub fn addStepDependencies(
    self: *const GhosttyResources,
    other_step: *std.Build.Step,
) void {
    _ = self;
    _ = other_step;
}
")))))))
            (add-after 'patch-el-files 'patch-guix-specific-shell-paths
              (lambda _
                ;; Keep local /bin/sh references patched to the store, but
                ;; do not leak store paths into remote TRAMP and Docker hosts.
                (substitute* "ghostel.el"
                  (("\\(\"docker\" \"[^\"]*/bin/sh\"\\)")
                   "(\"docker\" \"/bin/sh\")")
                  (("\\(list \"([^\"]*/bin/sh)\" \"-c\"" _ shell)
                   (string-append
                    "(list (if remote-p \"/bin/sh\" \""
                    shell "\") \"-c\"")))))
            (add-after 'patch-zig-dependencies 'build-native-module
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((root (dirname (getcwd))))
                  (with-directory-excursion root
                    (mkdir-p "zig-cache/global")
                    (mkdir-p "zig-cache/local")
                    (setenv "HOME" root)
                    (setenv "EMACS_INCLUDE_DIR"
                            (string-append (assoc-ref inputs "emacs")
                                           "/include"))
                    (setenv "ZIG_GLOBAL_CACHE_DIR"
                            (string-append root "/zig-cache/global"))
                    (setenv "ZIG_LOCAL_CACHE_DIR"
                            (string-append root "/zig-cache/local"))
                    (invoke "zig" "build"
                            "-Doptimize=ReleaseFast"
                            "-Dcpu=baseline")))))
            (add-after 'install 'install-resources
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (root (dirname (getcwd)))
                       (elpa-dir (elpa-directory out)))
                  (display "copy")
                  (copy-recursively (string-append root "/etc")
                                    (string-append elpa-dir "/etc"))
                  (display "install")
                  (install-file (string-append root
                                               "/zig-out/ghostel-module.so")
                                elpa-dir)))))))
      (native-inputs (list zig-0.15))
      ;; Needed for terminfo handling
      (propagated-inputs (list ncurses))
      (home-page "https://github.com/dakra/ghostel")
      (synopsis "Terminal emulator powered by libghostty")
      (description
       "Ghostel is an Emacs terminal emulator powered by @code{libghostty-vt},
the VT engine from Ghostty.  It uses a native Zig dynamic module for terminal
state and rendering, while Emacs Lisp manages buffers, processes, keymaps,
shell integration, and user commands.")
      (license (list license:gpl3+
                     license:expat
                     license:asl2.0
                     license:unicode)))))
