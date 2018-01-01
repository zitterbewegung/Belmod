public class TxHandler {

  /** Creates a public ledger whose current UTXOPool 
    * (collection of unspent transaction outputs) is utxoPool. 
    * This should  make a defensive copy of utxoPool by using 
    * the UTXOPool (UTXOPool uPool) constructor.
    */
  public TxHandler(UTXOPool utxoPool);

  /** Returns true if
   * (1) all outputs claimed by tx are in the current UTXO pool
   * (2) the signatures on each input of tx are valid
   * (3) no UTXO is claimed multiple times by tx
   * (4) all of tx’s output values are non-negative
   * (5) the sum of tx’s input values is greater than or equal 
   * to the sum of its output values; and false otherwise.
   */
  public boolean isValidTx(Transaction tx);

  /** Handles each epoch by receiving an unordered array of 
   * proposed transactions, checking each transaction for 
   * correctness, returning a mutually valid array of accepted 
   * transactions, and updating the current UTXO pool as 
   * appropriate.
   */
  public Transaction[] handleTxs(Transaction[] possibleTxs);

}
