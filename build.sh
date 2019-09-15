nix-build
rm -rf ./build
cp -r ./result/bin/game-solver-exe.jsexe ./build
sh copyStatic.sh
