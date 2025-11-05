'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { userService } from '@/services/userService';
import { transactionService } from '@/services/transactionService';
import TransactionList from '@/components/TransactionList';
import { Transaction } from '@/types/user';

export default function TransactionReportsPage() {
  const router = useRouter();
  const [currentDateTime, setCurrentDateTime] = useState(new Date());
  const [currentUser, setCurrentUser] = useState<any>(null);
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(false);
  const [reportType, setReportType] = useState<'monthly' | 'yearly' | 'custom' | null>(null);
  const [startDate, setStartDate] = useState('');
  const [endDate, setEndDate] = useState('');
  const [currentPage, setCurrentPage] = useState(0);
  const [totalPages, setTotalPages] = useState(0);
  const [hasNext, setHasNext] = useState(false);
  const [hasPrevious, setHasPrevious] = useState(false);

  useEffect(() => {
    const user = userService.getCurrentUser();
    if (!user || user.userType !== 'A') {
      router.push('/login');
      return;
    }
    setCurrentUser(user);

    const timer = setInterval(() => {
      setCurrentDateTime(new Date());
    }, 1000);

    return () => clearInterval(timer);
  }, [router]);

  const formatDateTime = (date: Date): string => {
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const year = date.getFullYear();
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    const seconds = String(date.getSeconds()).padStart(2, '0');
    return `${month}/${day}/${year} ${hours}:${minutes}:${seconds}`;
  };

  const handleMonthlyReport = async () => {
    try {
      setLoading(true);
      setReportType('monthly');
      const response = await transactionService.getMonthlyTransactions();
      setTransactions(response.transactions);
      setTotalPages(1);
      setHasNext(false);
      setHasPrevious(false);
    } catch (err: any) {
      alert(err.message || 'Failed to generate monthly report');
    } finally {
      setLoading(false);
    }
  };

  const handleYearlyReport = async () => {
    try {
      setLoading(true);
      setReportType('yearly');
      const response = await transactionService.getYearlyTransactions();
      setTransactions(response.transactions);
      setTotalPages(1);
      setHasNext(false);
      setHasPrevious(false);
    } catch (err: any) {
      alert(err.message || 'Failed to generate yearly report');
    } finally {
      setLoading(false);
    }
  };

  const handleCustomReport = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!startDate || !endDate) {
      alert('Please enter both start and end dates');
      return;
    }

    try {
      setLoading(true);
      setReportType('custom');
      const response = await transactionService.getTransactionsByDateRange(startDate, endDate);
      setTransactions(response);
      setTotalPages(1);
      setHasNext(false);
      setHasPrevious(false);
    } catch (err: any) {
      alert(err.message || 'Failed to generate custom report');
    } finally {
      setLoading(false);
    }
  };

  const handlePF3 = () => {
    router.push('/admin');
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
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 17v-2m3 2v-4m3 4v-6m2 10H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z" />
                </svg>
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">Transaction Reports</h1>
                <p className="text-xs text-gray-600">Transaction: CR00 | Program: CORPT00C</p>
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
          <div className="bg-white rounded-lg shadow-xl overflow-hidden mb-6">
            <div className="bg-gradient-to-r from-indigo-600 to-indigo-700 px-6 py-4">
              <h2 className="text-xl font-bold text-white">Generate Reports</h2>
              <p className="text-indigo-100 text-sm mt-1">Select report type or enter custom date range</p>
            </div>

            <div className="p-6">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
                <Button onClick={handleMonthlyReport} disabled={loading}>
                  Monthly Report
                </Button>
                <Button onClick={handleYearlyReport} disabled={loading}>
                  Yearly Report
                </Button>
              </div>

              <form onSubmit={handleCustomReport} className="border-t pt-6">
                <h3 className="text-lg font-semibold text-gray-900 mb-4">Custom Date Range</h3>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <Input
                    label="Start Date"
                    type="date"
                    value={startDate}
                    onChange={(e) => setStartDate(e.target.value)}
                    required
                  />
                  <Input
                    label="End Date"
                    type="date"
                    value={endDate}
                    onChange={(e) => setEndDate(e.target.value)}
                    required
                  />
                  <div className="flex items-end">
                    <Button type="submit" disabled={loading} className="w-full">
                      Generate Custom Report
                    </Button>
                  </div>
                </div>
              </form>
            </div>
          </div>

          {transactions.length > 0 && (
            <div className="bg-white rounded-lg shadow-xl overflow-hidden">
              <div className="bg-gradient-to-r from-green-600 to-green-700 px-6 py-4">
                <h2 className="text-xl font-bold text-white">
                  {reportType === 'monthly' && 'Monthly Report'}
                  {reportType === 'yearly' && 'Yearly Report'}
                  {reportType === 'custom' && 'Custom Report'}
                </h2>
                <p className="text-green-100 text-sm mt-1">{transactions.length} transactions found</p>
              </div>

              <div className="p-6">
                <TransactionList
                  transactions={transactions}
                  loading={loading}
                  currentPage={currentPage}
                  totalPages={totalPages}
                  hasNext={hasNext}
                  hasPrevious={hasPrevious}
                  onPageChange={setCurrentPage}
                />
              </div>
            </div>
          )}

          <div className="mt-6 bg-gray-50 px-6 py-4 border border-gray-200 rounded-lg">
            <div className="flex items-center justify-between">
              <div className="text-sm text-gray-600">
                <span className="font-semibold">PF3</span> = Return to Admin Menu
              </div>
              <Button variant="secondary" onClick={handlePF3}>PF3 - Admin Menu</Button>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
