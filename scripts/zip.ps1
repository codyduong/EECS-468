$toZip = $args[0].Trim('.\').Trim('\')

# Cleanup old zips
Remove-Item -Recurse -Force "CodyDuong_$toZip" -ErrorAction SilentlyContinue
Remove-Item -Force "CodyDuong_$toZip.zip" -ErrorAction SilentlyContinue
Copy-Item -Recurse -Force $toZip "CodyDuong_$toZip"
Compress-Archive -Path "CodyDuong_$toZip" -DestinationPath "CodyDuong_$toZip.zip"

# Cleanup temp
Remove-Item -Recurse -Force "CodyDuong_$toZip" -ErrorAction SilentlyContinue