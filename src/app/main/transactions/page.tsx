'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import { transactionService } from '@/services/transactionService';
import TransactionList from '@/components/TransactionList';
import { Transaction } from '@/types/user';

export default function TransactionsPage() {
  const router = useRouter();
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [currentUser, setCurrentUser] = useState<any>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrevious, setHasPrevious] = useState(false);
  const [searchTransactionId, setSearchTransactionId] = useState('');

  const pageSize = 10;

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user) {
      router.push('/login');
      return;
    }
    setCurrentUser(user);

    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, [router]);

  useEffect(() => {
    if (currentUser) {
      fetchTransactions();
    }
  }, [currentPage, currentUser]);

  const formatDateTime = (date: Date): string => {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}:${seconds}`;
  };

  const fetchTransactions = async () => {
    try {
      setLoading(true);
      const params: any = {
        page: currentPage,
        size: pageSize,
      };

      if (searchTransactionId.trim()) {
        params.startTransactionId = searchTransactionId.trim();
      }

      const response = await transactionService.getTransactions(params);
      setTransactions(response.content);
      setTotalPages(response.totalPages);
      setHasNext(response.hasNext);
      setHasPrevious(response.hasPrevious);
    } catch (err: any) {
      console.error('Error fetching transactions:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    setCurrentPage(0);
    fetchTransactions();
  };

  const handleClearSearch = () => {
    setSearchTransactionId('');
    setCurrentPage(0);
    fetchTransactions();
  };

  const handlePageChange = (page: number) => {
    setCurrentPage(page);
  };

  const handlePF3 = () => {
    router.push('/main');
  };

  if (!currentUser) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex flex-col">
      <header className="bg-white shadow-md">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
          <div className="flex justify-between items-center">
            <div className="flex items-center">
              <div className="flex items-center justify-center w-10 h-10 bg-indigo-600 rounded-lg mr-3">
                <svg className="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">Transaction List</h1>
                <p className="text-xs text-gray-600">Transaction: CT00 | Program: COTRN00C</p>
              </div>
            </div>
            <div className="text-right">
              <div className="text-sm text-gray-600 font-medium">{formatDateTime(currentDateTime)}</div>
              <div className="text-xs text-gray-500 mt-1">User: {currentUser.firstName} {currentUser.lastName}</div>
            </div>
          </div>
        </div>
      </header>

      <main className="flex-1 px-4 sm:px-6 lg:px-8 py-8">
        <div className="max-w-7xl mx-auto">
          <div className="bg-white rounded-lg shadow-xl overflow-hidden">
            <div className="bg-gradient-to-r from-indigo-600 to-indigo-700 px-6 py-4">
              <h2 className="text-xl font-bold text-white">Your Transactions</h2>
              <p className="text-indigo-100 text-sm mt-1">View your transaction history</p>
            </div>

            <div className="p-6">
              <form onSubmit={handleSearch} className="mb-6">
                <div className="flex gap-4 items-end">
                  <div className="flex-1 max-w-md">
                    <Input
                      label="Search by Transaction ID"
                      value={searchTransactionId}
                      onChange={(e) => setSearchTransactionId(e.target.value)}
                      placeholder="Enter Transaction ID"
                      maxLength={16}
                    />
                  </div>
                  <Button type="submit">Search</Button>
                  {searchTransactionId && (
                    <Button type="button" variant="secondary" onClick={handleClearSearch}>Clear</Button>
                  )}
                </div>
              </form>

              <TransactionList
                transactions={transactions}
                loading={loading}
                currentPage={currentPage}
                totalPages={totalPages}
                hasNext={hasNext}
                hasPrevious={hasPrevious}
                onPageChange={handlePageChange}
              />
            </div>

            <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
              <div className="flex items-center justify-between">
                <div className="text-sm text-gray-600">
                  <span className="font-semibold">PF3</span> = Return to Main Menu
                </div>
                <Button variant="secondary" onClick={handlePF3}>PF3 - Main Menu</Button>
              </div>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
