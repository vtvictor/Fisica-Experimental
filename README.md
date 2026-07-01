# Fisica-Experimental

Coleção de programas educativos em Fortran 90/95 para demonstração de conceitos de física experimental e métodos numéricos.

## Lista de programas

| Programa | Descrição |
|----------|-----------|
| `DerivadaPolinomio.f90` | Calcula os coeficientes da derivada de um polinômio a partir dos coeficientes fornecidos. |
| `DerivadasNumericas.f90` | Calcula a derivada numérica de uma função usando diferenças finitas centradas. |
| `IncertezasRelativas.f90` | Determina média, desvio padrão, incerteza da média e incerteza relativa de um conjunto de medições. |
| `IntegraisNumericas.f90` | Calcula a integral numérica de um conjunto de pontos (x,y) usando a regra do trapézio. |
| `Projetil.f90` | Determina alcance máximo, altura máxima e tempo de voo de um projétil (sem resistência do ar). |
| `OsciladorHarmonico.f90` | Calcula período e frequência de um oscilador massa‑mola e mostra a posição x(t) em alguns instantes. |
| `PenduloSimples.f90` | Calcula o período de um pêndulo simples para oscilações pequenas (T = 2π√(L/g)). |

## Como compilar e executar

Cada programa é um arquivo-fonte Fortran independente. Para compilar, use o compilador GNU Fortran (`gfortran`) ou qualquer outro compilador Fortran 90/95.

### Exemplo de compilação

```bash
gfortran -o DerivadaPolinomio DerivadaPolinomio.f90
./DerivadaPolinomio
```

### Compilar todos os programas (script opcional)

```bash
for f in *.f90; do
    gfortran -o "${f%.f90}" "$f"
done
```

Depois de compilar, execute o executável gerado (ex.: `./DerivadaPolinomio`) e siga as instruções na tela.

## Observações

- Os programas foram escritos com foco em clareade e didática, não em desempenho otimizado.
- Entradas são lidas via `stdin` (teclado) conforme indicado nas mensagens do programa.
- Qualquer dúvida ou sugestão, sinta-se à vontade para abrir uma issue ou enviar um pull request.

## Licença

Este repositório é destinado a fins educacionais. Sinta-se livre para usar, modificar e compartilhar o código.
