#genrule(
    #name = "settings-tool"
    #out = "settings.sh"
      #cmd = """
      #cat > $@ <<"EOF"
##! /bin/sh

#GHC_PATH = `stack path --compiler-bin`

#sed -e 's/@VERSION@/1/g' \
    #-e 's/@GHC_PATH@/$GHC_PATH/g' \

#EOF
    #"""
    #)

#genrule(
    #name = "settings",
    #srcs = [
        #"settings.in",
    #],
    #outs = [
        #"settings.bzl",
    #],
    #cmd = "$(location :settings-tool) < $(<) > $(@)",
    #tools = [":settings-tool"],

#)

