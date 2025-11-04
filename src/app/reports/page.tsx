'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { transactionService } from '@/services/transactionService';
import { Transaction } from '@/types/transaction';
import { Button, Input } from '@/components/ui';

type ReportType = 'monthly' | 'yearly' | 'custom' | '';

export default function ReportsPage() {
  const router = useRouter();
  const { isAuthenticated } = useAuth();
  const [reportType, setReportType] = useState<ReportType>('');
  const [startDate, setStartDate] = useState('');
  const [endDate, setEndDate] = useState('');
  const [transactions, setTransactions] = useState<Transaction[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, router]);

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };
    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  const validateCustomDates = () => {
    if (!startDate) {
      setError('Start Date - Month can NOT be empty...');
      return false;
    }
    if (!endDate) {
      setError('End Date - Month can NOT be empty...');
      return false;
    }
    
    const start = new Date(startDate);
    const end = new Date(endDate);
    
    if (isNaN(start.getTime())) {
      setError('Start Date - Not a valid date...');
      return false;
    }
    if (isNaN(end.getTime())) {
      setError('End Date - Not a valid date...');
      return false;
    }
    if (start > end) {
      setError('Start date must be before end date');
      return false;
    }
    
    return true;
  };

  const handleGenerateReport = async () => {
    if (!reportType) {
      setError('Select a report type to print report...');
      return;
    }

    if (reportType === 'custom' && !validateCustomDates()) {
      return;
    }

    try {
      setLoading(true);
      setError('');
      setSuccessMessage('');
      setTransactions([]);

      let response;
      let reportName = '';

      if (reportType === 'monthly') {
        response = await transactionService.getMonthlyTransactions();
        reportName = 'Monthly';
        setTransactions(response.transactions);
      } else if (reportType === 'yearly') {
        response = await transactionService.getYearlyTransactions();
        reportName = 'Yearly';
        setTransactions(response.transactions);
      } else if (reportType === 'custom') {
        const data = await transactionService.getTransactionsByDateRange({
          startDate,
          endDate,
        });
        reportName = 'Custom';
        setTransactions(data);
      }

      setSuccessMessage(`${reportName} report generated successfully`);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to generate report');
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setReportType('');
    setStartDate('');
    setEndDate('');
    setTransactions([]);
    setError('');
    setSuccessMessage('');
  };

  const formatAmount = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const calculateTotal = () => {
    return transactions.reduce((sum, t) => sum + t.transactionAmount, 0);
  };

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-purple-600 to-indigo-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CR00 - CORPT00C</h1>
              <p className="text-purple-100 text-sm">Transaction Report Generation</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-7xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          <h2 className="text-xl font-semibold text-gray-900 mb-6">Generate Transaction Report</h2>

          {error && (
            <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {error}
            </div>
          )}

          {successMessage && (
            <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg text-green-800">
              {successMessage}
            </div>
          )}

          <div className="space-y-6 mb-6">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-3">
                Select Report Type <span className="text-red-500">*</span>
              </label>
              <div className="space-y-3">
                <label className="flex items-center p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
                  <input
                    type="radio"
                    name="reportType"
                    value="monthly"
                    checked={reportType === 'monthly'}
                    onChange={(e) => {
                      setReportType(e.target.value as ReportType);
                      setError('');
                    }}
                    className="w-4 h-4 text-purple-600"
                  />
                  <span className="ml-3 text-gray-900 font-medium">Monthly Report</span>
                  <span className="ml-auto text-sm text-gray-600">Current month transactions</span>
                </label>

                <label className="flex items-center p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
                  <input
                    type="radio"
                    name="reportType"
                    value="yearly"
                    checked={reportType === 'yearly'}
                    onChange={(e) => {
                      setReportType(e.target.value as ReportType);
                      setError('');
                    }}
                    className="w-4 h-4 text-purple-600"
                  />
                  <span className="ml-3 text-gray-900 font-medium">Yearly Report</span>
                  <span className="ml-auto text-sm text-gray-600">Current year transactions</span>
                </label>

                <label className="flex items-center p-4 border-2 rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
                  <input
                    type="radio"
                    name="reportType"
                    value="custom"
                    checked={reportType === 'custom'}
                    onChange={(e) => {
                      setReportType(e.target.value as ReportType);
                      setError('');
                    }}
                    className="w-4 h-4 text-purple-600"
                  />
                  <span className="ml-3 text-gray-900 font-medium">Custom Date Range</span>
                  <span className="ml-auto text-sm text-gray-600">Specify date range</span>
                </label>
              </div>
            </div>

            {reportType === 'custom' && (
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4 p-4 bg-purple-50 border border-purple-200 rounded-lg">
                <div>
                  <Input
                    label="Start Date"
                    type="date"
                    value={startDate}
                    onChange={(e) => setStartDate(e.target.value)}
                    required
                  />
                </div>
                <div>
                  <Input
                    label="End Date"
                    type="date"
                    value={endDate}
                    onChange={(e) => setEndDate(e.target.value)}
                    required
                  />
                </div>
              </div>
            )}

            <div className="flex gap-4">
              <Button onClick={handleGenerateReport} disabled={loading} className="flex-1">
                {loading ? 'Generating Report...' : 'Generate Report (PF5)'}
              </Button>
              <Button variant="secondary" onClick={handleClear} className="flex-1">
                Clear (PF4)
              </Button>
            </div>
          </div>

          {transactions.length > 0 && (
            <div className="mt-8 border-t border-gray-200 pt-6">
              <div className="flex justify-between items-center mb-4">
                <h3 className="text-lg font-semibold text-gray-900">Report Results</h3>
                <div className="text-right">
                  <p className="text-sm text-gray-600">Total Transactions: {transactions.length}</p>
                  <p className="text-lg font-bold text-green-600">Total Amount: {formatAmount(calculateTotal())}</p>
                </div>
              </div>

              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Transaction ID</th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Date</th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase">Description</th>
                      <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase">Amount</th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {transactions.map((transaction) => (
                      <tr key={transaction.transactionId} className="hover:bg-gray-50">
                        <td className="px-6 py-4 text-sm font-medium text-gray-900">{transaction.transactionId}</td>
                        <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDate}</td>
                        <td className="px-6 py-4 text-sm text-gray-900">{transaction.transactionDescription}</td>
                        <td className="px-6 py-4 text-sm text-right font-medium text-gray-900">
                          {formatAmount(transaction.transactionAmount)}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            </div>
          )}

          <div className="mt-6 pt-6 border-t border-gray-200">
            <Button variant="secondary" onClick={() => router.push('/menu')}>
              PF3 - Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
