'use client';

import React, { useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { billPaymentService } from '@/services/billPaymentService';
import { Transaction } from '@/types/bill-payment';
import { Input, Button, Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui';

export default function BillPaymentHistoryPage() {
  const router = useRouter();
  const [accountId, setAccountId] = useState('');
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [searched, setSearched] = useState(false);

  const handleSearch = useCallback(async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!accountId.trim()) {
      setError('Acct ID can NOT be empty...');
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setSearched(true);

      const data = await billPaymentService.getTransactionsByAccount(accountId);
      
      const billPayments = data.filter(
        (t) => t.transactionTypeCode === '02' && t.transactionCategoryCode === 2
      );
      
      setTransactions(billPayments);
    } catch (err: any) {
      setError(err.message || 'Failed to fetch transaction history');
      setTransactions([]);
    } finally {
      setLoading(false);
    }
  }, [accountId]);

  const handleClear = useCallback(() => {
    setAccountId('');
    setTransactions([]);
    setError(null);
    setSearched(false);
  }, []);

  const handleViewDetails = useCallback((transactionId: number) => {
    router.push(`/bill-payment/transaction/${transactionId}`);
  }, [router]);

  return (
    <div className="p-6 max-w-7xl mx-auto">
      <div className="mb-6">
        <div className="flex justify-between items-center mb-4">
          <h1 className="text-3xl font-bold">Bill Payment History</h1>
          <Button variant="secondary" onClick={() => router.push('/bill-payment')}>
            Back to Bill Payment
          </Button>
        </div>
        <p className="text-gray-600">
          View your bill payment transaction history
        </p>
      </div>

      {/* Error Message */}
      {error && (
        <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg">
          <p className="text-red-800 font-semibold">{error}</p>
        </div>
      )}

      {/* Search Form */}
      <form onSubmit={handleSearch} className="bg-white shadow rounded-lg p-6 mb-6">
        <div className="flex gap-4 items-end">
          <div className="flex-1">
            <Input
              label="Account ID"
              value={accountId}
              onChange={(e) => setAccountId(e.target.value)}
              placeholder="Enter Account ID (11 characters)"
              maxLength={11}
              required
              disabled={loading}
            />
          </div>
          <div className="flex gap-2">
            <Button type="submit" disabled={loading}>
              {loading ? 'Searching...' : 'Search'}
            </Button>
            <Button
              type="button"
              variant="secondary"
              onClick={handleClear}
              disabled={loading}
            >
              Clear
            </Button>
          </div>
        </div>
      </form>

      {/* Transaction History Table */}
      {searched && (
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-xl font-semibold mb-4">
            Transaction History for Account: {accountId}
          </h2>
          
          {loading ? (
            <div className="text-center py-8 text-gray-500">Loading...</div>
          ) : transactions.length === 0 ? (
            <div className="text-center py-8 text-gray-500">
              No bill payment transactions found for this account
            </div>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Transaction ID</TableHead>
                  <TableHead>Date & Time</TableHead>
                  <TableHead>Amount</TableHead>
                  <TableHead>Card Number</TableHead>
                  <TableHead>Description</TableHead>
                  <TableHead>Status</TableHead>
                  <TableHead>Actions</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {transactions.map((transaction) => (
                  <TableRow key={transaction.transactionId}>
                    <TableCell>
                      <div className="cursor-pointer font-mono text-sm" onClick={() => handleViewDetails(transaction.transactionId)}>
                        {transaction.transactionId}
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="cursor-pointer" onClick={() => handleViewDetails(transaction.transactionId)}>
                        {new Date(transaction.processingTimestamp).toLocaleString()}
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="cursor-pointer font-semibold" onClick={() => handleViewDetails(transaction.transactionId)}>
                        ${transaction.amount.toFixed(2)}
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="cursor-pointer font-mono text-sm" onClick={() => handleViewDetails(transaction.transactionId)}>
                        {transaction.cardNumber.replace(/(\d{4})(\d{4})(\d{4})(\d{4})/, '$1 $2 $3 $4')}
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="cursor-pointer text-sm" onClick={() => handleViewDetails(transaction.transactionId)}>
                        {transaction.description}
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="cursor-pointer" onClick={() => handleViewDetails(transaction.transactionId)}>
                        <span className="px-2 py-1 rounded text-xs font-semibold bg-green-100 text-green-800">
                          Completed
                        </span>
                      </div>
                    </TableCell>
                    <TableCell>
                      <Button
                        size="sm"
                        onClick={(e) => {
                          e.stopPropagation();
                          handleViewDetails(transaction.transactionId);
                        }}
                      >
                        View Details
                      </Button>
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}

          {transactions.length > 0 && (
            <div className="mt-4 text-sm text-gray-600">
              Total Transactions: {transactions.length}
            </div>
          )}
        </div>
      )}

      {/* Instructions */}
      {!searched && (
        <div className="bg-gray-50 border border-gray-200 rounded-lg p-4">
          <h3 className="font-semibold text-gray-700 mb-2">Instructions:</h3>
          <ul className="text-sm text-gray-600 space-y-1 list-disc list-inside">
            <li>Enter an Account ID to view bill payment transaction history</li>
            <li>Click on any transaction row to view detailed information</li>
            <li>Use the "View Details" button to see complete transaction information</li>
            <li>Only bill payment transactions (Type 02, Category 2) are displayed</li>
          </ul>
        </div>
      )}
    </div>
  );
}
