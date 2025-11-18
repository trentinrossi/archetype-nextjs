'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Button } from '@/components/ui';

export default function TransactionDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [transaction, setTransaction] = useState<Transaction | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (params.id) {
      fetchTransaction(params.id as string);
    }
  }, [params.id]);

  const fetchTransaction = async (id: string) => {
    try {
      setLoading(true);
      setError(null);
      const data = await transactionService.getTransactionById(id);
      setTransaction(data);
    } catch (err) {
      setError('Transaction not found');
      console.error('Failed to load transaction:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this transaction?')) return;
    
    try {
      await transactionService.deleteTransaction(params.id as string);
      router.push('/transactions');
    } catch (err) {
      alert('Failed to delete transaction');
      console.error(err);
    }
  };

  if (loading) {
    return <div className="p-6">Loading transaction details...</div>;
  }

  if (error || !transaction) {
    return (
      <div className="p-6">
        <div className="mb-4 p-4 bg-red-50 border border-red-200 text-red-700 rounded">
          {error || 'Transaction not found'}
        </div>
        <Button variant="secondary" onClick={() => router.push('/transactions')}>
          Back to List
        </Button>
      </div>
    );
  }

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Transaction Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/transactions/${transaction.transactionId}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/transactions')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction ID</label>
            <p className="text-gray-900 font-mono">{transaction.transactionId}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction Date</label>
            <p className="text-gray-900">{transaction.formattedTransactionDate}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction Amount</label>
            <p className="text-gray-900 font-mono text-lg">{transaction.formattedTransactionAmount}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Card Number</label>
            <p className="text-gray-900 font-mono">{transaction.tranCardNum}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction Type Code</label>
            <p className="text-gray-900">{transaction.tranTypeCd}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction Category Code</label>
            <p className="text-gray-900">{transaction.tranCatCd}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700 mb-1">Transaction Source</label>
            <p className="text-gray-900">{transaction.tranSource}</p>
          </div>
          
          <div className="md:col-span-2">
            <label className="block text-sm font-semibold text-gray-700 mb-1">Description</label>
            <p className="text-gray-900">{transaction.tranDesc}</p>
          </div>
        </div>

        <div className="border-t pt-6">
          <h2 className="text-lg font-semibold mb-4">Merchant Information</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Merchant ID</label>
              <p className="text-gray-900">{transaction.tranMerchantId}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Merchant Name</label>
              <p className="text-gray-900">{transaction.tranMerchantName}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Merchant City</label>
              <p className="text-gray-900">{transaction.tranMerchantCity}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Merchant Zip</label>
              <p className="text-gray-900">{transaction.tranMerchantZip}</p>
            </div>
          </div>
        </div>

        <div className="border-t pt-6">
          <h2 className="text-lg font-semibold mb-4">Transaction Timestamps</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Original Date</label>
              <p className="text-gray-900">{transaction.tranOrigTs}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Processing Date</label>
              <p className="text-gray-900">{transaction.tranProcTs}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Created At</label>
              <p className="text-gray-900">{new Date(transaction.createdAt).toLocaleString()}</p>
            </div>
            
            <div>
              <label className="block text-sm font-semibold text-gray-700 mb-1">Updated At</label>
              <p className="text-gray-900">{new Date(transaction.updatedAt).toLocaleString()}</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
