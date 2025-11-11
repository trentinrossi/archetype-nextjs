'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { accountService } from '@/services/accountService';
import { transactionService } from '@/services/transactionService';
import { cardCrossReferenceService } from '@/services/cardCrossReferenceService';
import { Account } from '@/types/account';
import { Transaction } from '@/types/transaction';
import { CardCrossReference } from '@/types/card-cross-reference';
import { Button, Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui';

export default function AccountDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [account, setAccount] = useState<Account | null>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [cards, setCards] = useState<CardCrossReference[]>([]);
  const [loading, setLoading] = useState(true);

  const fetchAccountData = useCallback(async () => {
    if (!params.accountId) return;
    
    try {
      const accountId = params.accountId as string;
      const [accountData, transactionsData, cardsData] = await Promise.all([
        accountService.getAccountById(accountId),
        transactionService.getTransactionsByAccount(accountId),
        cardCrossReferenceService.getCardCrossReferencesByAccount(accountId)
      ]);
      
      setAccount(accountData);
      setTransactions(transactionsData);
      setCards(cardsData);
    } catch (err) {
      console.error('Failed to load account data:', err);
    } finally {
      setLoading(false);
    }
  }, [params.accountId]);

  useEffect(() => {
    fetchAccountData();
  }, [fetchAccountData]);

  const handleDelete = async () => {
    if (!account) return;
    if (!confirm(`Are you sure you want to delete account ${account.accountId}?`)) return;
    
    try {
      await accountService.deleteAccount(account.accountId);
      router.push('/accounts');
    } catch (err) {
      alert('Failed to delete account');
      console.error(err);
    }
  };

  const handlePayBill = () => {
    if (account) {
      router.push(`/bill-payment?accountId=${account.accountId}`);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!account) return <div className="p-6">Account not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Account Details</h1>
        <div className="flex gap-2">
          {account.hasPositiveBalance && (
            <Button onClick={handlePayBill}>
              Pay Bill
            </Button>
          )}
          <Button onClick={() => router.push(`/accounts/${account.accountId}/edit`)}>
            Edit
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/accounts')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-xl font-semibold mb-4">Account Information</h2>
        <div className="grid grid-cols-2 gap-4">
          <div>
            <label className="block text-sm font-semibold text-gray-700">Account ID</label>
            <p className="mt-1 text-gray-900">{account.accountId}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Current Balance</label>
            <p className="mt-1 text-gray-900 text-lg font-bold">
              ${account.currentBalance.toFixed(2)}
            </p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Status</label>
            <p className="mt-1">
              <span className={`px-2 py-1 rounded text-xs font-semibold ${
                account.hasPositiveBalance 
                  ? 'bg-yellow-100 text-yellow-800' 
                  : 'bg-green-100 text-green-800'
              }`}>
                {account.hasPositiveBalance ? 'Balance Due' : 'Paid'}
              </span>
            </p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Created At</label>
            <p className="mt-1 text-gray-900">{new Date(account.createdAt).toLocaleString()}</p>
          </div>
          
          <div>
            <label className="block text-sm font-semibold text-gray-700">Updated At</label>
            <p className="mt-1 text-gray-900">{new Date(account.updatedAt).toLocaleString()}</p>
          </div>
        </div>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <h2 className="text-xl font-semibold mb-4">Linked Cards ({cards.length})</h2>
        {cards.length === 0 ? (
          <p className="text-gray-500">No cards linked to this account</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Card Number</TableHead>
                <TableHead>Created At</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {cards.map((card) => (
                <TableRow key={card.cardNumber}>
                  <TableCell>{card.cardNumber}</TableCell>
                  <TableCell>{new Date(card.createdAt).toLocaleString()}</TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </div>

      <div className="bg-white shadow rounded-lg p-6">
        <h2 className="text-xl font-semibold mb-4">Transaction History ({transactions.length})</h2>
        {transactions.length === 0 ? (
          <p className="text-gray-500">No transactions found</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Transaction ID</TableHead>
                <TableHead>Description</TableHead>
                <TableHead>Amount</TableHead>
                <TableHead>Date</TableHead>
                <TableHead>Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {transactions.map((transaction) => (
                <TableRow key={transaction.transactionId}>
                  <TableCell>
                    <div className="cursor-pointer font-medium" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                      {transaction.transactionId}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                      {transaction.description}
                    </div>
                  </TableCell>
                  <TableCell>
                    <div className="cursor-pointer" onClick={() => router.push(`/transactions/${transaction.transactionId}`)}>
                      ${transaction.amount.toFixed(2)}
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
              ))}
            </TableBody>
          </Table>
        )}
      </div>
    </div>
  );
}
