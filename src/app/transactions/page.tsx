'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function TransactionsPage() {
  const router = useRouter();
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);
  const pageSize = 20;

  const fetchTransactions = useCallback(async (page: number = 0) => {
    try {
      setLoading(true);
      const data = await transactionService.getTransactions(page, pageSize, 'processingTimestamp,desc');
      setTransactions(data.content);
      setCurrentPage(data.number);
      setTotalPages(data.totalPages);
      setTotalElements(data.totalElements);
      setError(null);
    } catch (err) {
      setError('Failed to load transactions');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchTransactions(currentPage);
  }, [fetchTransactions, currentPage]);

  const handlePreviousPage = () => {
    if (currentPage > 0) {
      setCurrentPage(currentPage - 1);
    }
  };

  const handleNextPage = () => {
    if (currentPage < totalPages - 1) {
      setCurrentPage(currentPage + 1);
    }
  };

  const getTransactionTypeBadge = (typeCode: string) => {
    const types: Record<string, { label: string; color: string }> = {
      '02': { label: 'Bill Payment', color: 'bg-blue-100 text-blue-800' },
      '01': { label: 'Purchase', color: 'bg-green-100 text-green-800' },
      '03': { label: 'Refund', color: 'bg-purple-100 text-purple-800' },
    };
    
    const type = types[typeCode] || { label: typeCode, color: 'bg-gray-100 text-gray-800' };
    return (
      <span className={`px-2 py-1 rounded text-xs font-semibold ${type.color}`}>
        {type.label}
      </span>
    );
  };

  if (loading && transactions.length === 0) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Transactions</h1>
          <p className="text-gray-600 mt-1">
            Showing {transactions.length} of {totalElements} transactions
          </p>
        </div>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Transaction ID</TableHead>
            <TableHead>Type</TableHead>
            <TableHead>Description</TableHead>
            <TableHead>Amount</TableHead>
            <TableHead>Account ID</TableHead>
            <TableHead>Date</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {transactions.length === 0 ? (
            <TableRow>
              <TableCell colSpan={7} className="text-center text-gray-500 py-8">
                No transactions found
              </TableCell>
            </TableRow>
          ) : (
            transactions.map((transaction) => (
              <TableRow key={transaction.transactionId}>
                <TableCell>
                  <div className="cursor-pointer font-medium" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                    {transaction.transactionId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                    {getTransactionTypeBadge(transaction.transactionTypeCode)}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                    {transaction.description}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-semibold" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                    ${transaction.amount ? transaction.amount.toFixed(2) : '0.00'}
                  </div>
                </TableCell>
                <TableCell>
                  <div 
                    className="cursor-pointer text-blue-600 hover:text-blue-800" 
                    onClick={() => router.push(`/accounts/${transaction.accountId}`)}
                  >
                    {transaction.accountId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                    {new Date(transaction.processingTimestamp).toLocaleString()}
                  </div>
                </TableCell>
                <TableCell>
                  <Button 
                    size="sm" 
                    onClick={(e) => {
                      e.stopPropagation();
                      router.push(`/transactions/${transaction.transactionId}`);
                    }}
                  >
                    View
                  </Button>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>

      {totalPages > 1 && (
        <div className="flex justify-between items-center mt-6">
          <Button 
            variant="secondary" 
            onClick={handlePreviousPage} 
            disabled={currentPage === 0}
          >
            Previous
          </Button>
          <span className="text-gray-600">
            Page {currentPage + 1} of {totalPages}
          </span>
          <Button 
            variant="secondary" 
            onClick={handleNextPage} 
            disabled={currentPage >= totalPages - 1}
          >
            Next
          </Button>
        </div>
      )}
    </div>
  );
}
