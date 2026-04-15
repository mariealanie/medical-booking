#!/bin/bash

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║                    PROJECT STATISTICS                        ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

echo "📏 ПРОВЕРКА ДЛИНЫ СТРОК (>80 символов)"
echo "========================================"
violations=0
for file in src/*.hs app/*.hs; do
    if [ -f "$file" ]; then
        long_lines=$(grep -n "^" "$file" | awk 'length($0) > 80 {print NR}' | wc -l)
        if [ $long_lines -gt 0 ]; then
            echo "❌ $file: $long_lines строк >80 символов"
            grep -n "^" "$file" | awk 'length($0) > 80 {print "  Line " NR ": " length($0) " chars"}'
            violations=$((violations + 1))
        else
            echo "✅ $file: все строки ≤80 символов"
        fi
    fi
done

echo ""
echo "📊 ПОДСЧЁТ ФУНКЦИЙ"
echo "========================================"
total=0
for file in src/*.hs app/*.hs; do
    if [ -f "$file" ]; then
        count=$(grep -c "::" "$file")
        echo "  $file: $count"
        total=$((total + count))
    fi
done
echo "  ────────────────────"
echo "  ВСЕГО ФУНКЦИЙ: $total"

echo ""
echo "📦 ПОДСЧЁТ ТИПОВ ДАННЫХ"
echo "========================================"
for file in src/*.hs app/*.hs; do
    if [ -f "$file" ]; then
        data=$(grep -c "^data " "$file")
        newtype=$(grep -c "^newtype " "$file")
        type_syn=$(grep -c "^type " "$file")
        if [ $((data + newtype + type_syn)) -gt 0 ]; then
            echo "  $file: data=$data, newtype=$newtype, type=$type_syn"
        fi
    done
done

echo ""
echo "📝 ПРОВЕРКА ИМПОРТОВ"
echo "========================================"
for file in src/*.hs app/*.hs; do
    if [ -f "$file" ]; then
        imports=$(grep -c "^import " "$file")
        echo "  $file: $imports импортов"
    fi
done

echo ""
if [ $violations -eq 0 ]; then
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  ✅ ВСЕ ПРОВЕРКИ ПРОЙДЕНЫ - СТРОК ≤80 СИМВОЛОВ              ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
else
    echo "╔══════════════════════════════════════════════════════════════╗"
    echo "║  ❌ НАЙДЕНО $violations ФАЙЛОВ С ДЛИННЫМИ СТРОКАМИ              ║"
    echo "╚══════════════════════════════════════════════════════════════╝"
fi
