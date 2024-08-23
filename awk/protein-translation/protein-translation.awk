BEGIN {
    FPAT="..?.?"
    OFS=" "
}
      {
          result = ""
          stop = 0
          for (i = 1; i <= NF; i++) {
              protein = translate($i)
              if (protein == "STOP") {
                  NF = i-1
              } else {
                  $i = protein
              }
          }
          print
      }

      function translate(codon)
      {
              switch (codon) {
              case "AUG":
                  return "Methionine"
              case "UUU":
              case "UUC":
                  return "Phenylalanine"
              case "UUA":
              case "UUG":
                  return "Leucine"
              case "UCU":
              case "UCC":
              case "UCA":
              case "UCG":
                  return "Serine"
              case "UAU":
              case "UAC":
                  return "Tyrosine"
              case "UGU":
              case "UGC":
                  return "Cysteine"
              case "UGG":
                  return "Tryptophan"
              case "UAA":
              case "UAG":
              case "UGA":
                  return "STOP"
              default:
                  print "Invalid codon"
                  exit 1
              }
      }
