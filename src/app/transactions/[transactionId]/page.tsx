'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Button } from '@/components/ui';

export default function TransactionDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [transaction, setTransaction] = useState<Transaction | null>(null);
  const [loading, setLoading] = useState(true);

  const fetchTransaction = useCallback(async () => {
    if (!params.transactionId) return;
    
    try {
      const transactionId = Number(params.transactionId);
      const data = await transactionService.getTransactionById(transactionId);
      setTransaction(data);
    } catch (err) {
      console.error('Failed to load transaction:', err);
    } finally {
      setLoading(false);
    }
  }, [params.transactionId]);

  useEffect(() => {
    fetchTransaction();
  }, [fetchTransaction]);

  const getTransactionTypeLabel = (typeCode: string) => {
    const types: Record<string, string> = {
      '02': 'Bill Payment',
      '01': 'Purchase',
      '03': 'Refund',
    };
    return types[typeCode] || typeCode;
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!transaction) return <div className="p-6">Transaction not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Transaction Details</h1>
        <div className="flex gap-2">
          <Button 
            variant="secondary" 
            onClick={() => router.push(`/accounts/${transaction.accountId}`)}
          >
            View Account
          </Button>
          <Button variant="secondary" onClick={() => router.push('/transactions')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-xl font-semibold mb-4">Transaction Information</h2>
        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Transaction ID</label>
            <p className="mt-1 text-gray-900 font-mono">{transaction.transactionId}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Amount</label>
            <p className="mt-1 text-gray-900 text-lg font-bold">
              ${transaction.amount.toFixed(2)}
            </p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Transaction Type</label>
            <p className="mt-1 text-gray-900">
              {getTransactionTypeLabel(transaction.transactionTypeCode)} ({transaction.transactionTypeCode})
            </p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Category Code</label>
            <p className="mt-1 text-gray-900">{transaction.transactionCategoryCode}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Description</label>
            <p className="mt-1 text-gray-900">{transaction.description}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Transaction Source</label>
            <p className="mt-1 text-gray-900">{transaction.transactionSource}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Account ID</label>
            <p className="mt-1">
              <span 
                className="text-blue-600 hover:text-blue-800 cursor-pointer"
                onClick={() => router.push(`/accounts/${transaction.accountId}`)}
              >
                {transaction.accountId}
              </span>
            </p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Card Number</label>
            <p className="mt-1 text-gray-900 font-mono">{transaction.cardNumber}</p>
          </div>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-xl font-semibold mb-4">Merchant Information</h2>
        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Merchant ID</label>
            <p className="mt-1 text-gray-900">{transaction.merchantId}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Merchant Name</label>
            <p className="mt-1 text-gray-900">{transaction.merchantName}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Merchant City</label>
            <p className="mt-1 text-gray-900">{transaction.merchantCity}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Merchant ZIP</label>
            <p className="mt-1 text-gray-900">{transaction.merchantZip}</p>
          </div>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6">
        <h2 className="text-xl font-semibold mb-4">Timestamps</h2>
        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Origination Timestamp</label>
            <p className="mt-1 text-gray-900">{new Date(transaction.originationTimestamp).toLocaleString()}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Processing Timestamp</label>
            <p className="mt-1 text-gray-900">{new Date(transaction.processingTimestamp).toLocaleString()}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Created At</label>
            <p className="mt-1 text-gray-900">{new Date(transaction.createdAt).toLocaleString()}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Updated At</label>
            <p className="mt-1 text-gray-900">{new Date(transaction.updatedAt).toLocaleString()}</p>
          </div>
        </div>
      </div>
    </div>
  );
}
