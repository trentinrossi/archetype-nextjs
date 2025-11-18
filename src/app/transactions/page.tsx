'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction, TransactionListPage } from '@/types/transaction';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button, Input } from '@/components/ui';

export default function TransactionsPage() {
  const router = useRouter();
  const [transactionData, setTransactionData] = useState<TransactionListPage | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [searchId, setSearchId] = useState('');
  const [currentPage, setCurrentPage] = useState(1);

  const fetchTransactions = useCallback(async (page: number = 1) => {
    try {
      setLoading(true);
      setError(null);
      const data = await transactionService.getTransactionPage(page);
      setTransactionData(data);
      setCurrentPage(page);
    } catch (err) {
      setError('Failed to load transactions');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchTransactions(1);
  }, [fetchTransactions]);

  const handleSearch = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!searchId.trim()) {
      fetchTransactions(1);
      return;
    }

    try {
      setLoading(true);
      setError(null);
      const data = await transactionService.searchTransactions({ 
        startTransactionId: searchId,
        pageNumber: 1 
      });
      setTransactionData(data);
      setCurrentPage(1);
    } catch (err) {
      setError('Transaction not found');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleNextPage = async () => {
    if (!transactionData?.hasNextPage || !transactionData.content.length) return;

    try {
      setLoading(true);
      setError(null);
      const lastTransaction = transactionData.content[transactionData.content.length - 1];
      const data = await transactionService.navigateForward(lastTransaction.transactionId);
      setTransactionData(data);
      setCurrentPage(prev => prev + 1);
    } catch (err) {
      setError('End of file reached');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handlePreviousPage = async () => {
    if (!transactionData?.hasPreviousPage || !transactionData.content.length || currentPage <= 1) return;

    try {
      setLoading(true);
      setError(null);
      const firstTransaction = transactionData.content[0];
      const data = await transactionService.navigateBackward(firstTransaction.transactionId, currentPage);
      setTransactionData(data);
      setCurrentPage(prev => prev - 1);
    } catch (err) {
      setError('Beginning of file reached');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleViewTransaction = (transactionId: string) => {
    router.push(`/transactions/${transactionId}`);
  };

  const handleDelete = async (id: string, e: React.MouseEvent) => {
    e.stopPropagation();
    if (!confirm('Are you sure you want to delete this transaction?')) return;
    
    try {
      await transactionService.deleteTransaction(id);
      fetchTransactions(currentPage);
    } catch (err) {
      alert('Failed to delete transaction');
      console.error(err);
    }
  };

  if (loading && !transactionData) {
    return <div className="p-6">Loading transactions...</div>;
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Transaction List</h1>
          <p className="text-sm text-gray-600 mt-1">
            Page {transactionData?.pageNumber || currentPage} of {transactionData?.totalPages || '?'}
          </p>
        </div>
        <Button onClick={() => router.push('/transactions/new')}>
          Add Transaction
        </Button>
      </div>

      <form onSubmit={handleSearch} className="mb-6 flex gap-2">
        <Input
          placeholder="Search by Transaction ID"
          value={searchId}
          onChange={(e) => setSearchId(e.target.value)}
          className="max-w-md"
        />
        <Button type="submit">Search</Button>
        {searchId && (
          <Button 
            type="button" 
            variant="secondary" 
            onClick={() => {
              setSearchId('');
              fetchTransactions(1);
            }}
          >
            Clear
          </Button>
        )}
      </form>

      {error && (
        <div className="mb-4 p-3 bg-red-50 border border-red-200 text-red-700 rounded">
          {error}
        </div>
      )}
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Transaction ID</TableHead>
            <TableHead>Date</TableHead>
            <TableHead>Description</TableHead>
            <TableHead>Amount</TableHead>
            <TableHead>Card Number</TableHead>
            <TableHead>Merchant</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {!transactionData?.content || transactionData.content.length === 0 ? (
            <TableRow>
              <TableCell colSpan={7} className="text-center text-gray-500 py-8">
                No transactions found
              </TableCell>
            </TableRow>
          ) : (
            transactionData.content.map((transaction) => (
              <TableRow key={transaction.transactionId}>
                <TableCell>
                  <div 
                    className="cursor-pointer text-blue-600 hover:text-blue-800" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.transactionId}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.formattedTransactionDate}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.tranDesc}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer font-mono" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.formattedTransactionAmount}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.tranCardNum}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer" 
                    onClick={() => handleViewTransaction(transaction.transactionId)}
                  >
                    {transaction.tranMerchantName}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/transactions/${transaction.transactionId}/edit`);
                      }}
                    >
                      Edit
                    </Button>
                    <Button 
                      size="sm" 
                      variant="danger" 
                      onClick={(e) => handleDelete(transaction.transactionId, e)}
                    >
                      Delete
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>

      <div className="mt-6 flex justify-between items-center">
        <Button
          variant="secondary"
          onClick={handlePreviousPage}
          disabled={loading || !transactionData?.hasPreviousPage || currentPage <= 1}
        >
          Previous Page
        </Button>
        
        <span className="text-sm text-gray-600">
          {transactionData?.totalElements || 0} total transactions
        </span>
        
        <Button
          variant="secondary"
          onClick={handleNextPage}
          disabled={loading || !transactionData?.hasNextPage}
        >
          Next Page
        </Button>
      </div>
    </div>
  );
}
