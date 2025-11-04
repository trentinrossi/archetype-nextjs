'use client';

import React, { useState } from 'react';
import { InterestCalculation } from '@/types/cardDemo';
import { interestService } from '@/services/interestService';

export default function InterestCalculationPage() {
  const [processingDate, setProcessingDate] = useState<string>(
    new Date().toISOString().split('T')[0]
  );
  const [results, setResults] = useState<InterestCalculation[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [calculated, setCalculated] = useState(false);

  const handleCalculate = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await interestService.calculateInterest(processingDate);
      setResults(data);
      setCalculated(true);
    } catch (err) {
      setError('Failed to calculate interest');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const formatCurrency = (value: string) => {
    const num = parseFloat(value);
    return isNaN(num) ? value : `$${num.toLocaleString('en-US', { minimumFractionDigits: 2, maximumFractionDigits: 2 })}`;
  };

  const formatPercentage = (value: string) => {
    const num = parseFloat(value);
    return isNaN(num) ? value : `${num.toFixed(2)}%`;
  };

  const totalInterest = results.reduce((sum, result) => {
    return sum + parseFloat(result.monthlyInterest);
  }, 0);

  return (
    <div className="p-6">
      <div className="mb-6">
        <h1 className="text-2xl font-bold">Interest Calculation</h1>
        <p className="text-gray-600 mt-1">Calculate monthly interest for all accounts</p>
      </div>

      <div className="bg-white shadow rounded-lg p-6 mb-6">
        <div className="max-w-md">
          <label className="block text-sm font-medium text-gray-700 mb-2">
            Processing Date
          </label>
          <input
            type="date"
            value={processingDate}
            onChange={(e) => setProcessingDate(e.target.value)}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
          />
          <p className="text-sm text-gray-500 mt-2">
            Select the date for which to calculate interest charges
          </p>
        </div>

        <div className="mt-6">
          <button
            onClick={handleCalculate}
            disabled={loading || !processingDate}
            className="px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 disabled:bg-gray-400 disabled:cursor-not-allowed"
          >
            {loading ? 'Calculating...' : 'Calculate Interest'}
          </button>
        </div>

        {error && (
          <div className="mt-4 bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-800 font-medium">Error: {error}</p>
          </div>
        )}
      </div>

      {loading && (
        <div className="flex items-center justify-center py-12">
          <div className="text-center">
            <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto mb-4"></div>
            <p className="text-gray-600">Calculating interest for all accounts...</p>
          </div>
        </div>
      )}

      {calculated && !loading && results.length === 0 && (
        <div className="bg-gray-50 border border-gray-200 rounded-lg p-8 text-center">
          <p className="text-gray-600">No interest calculations generated.</p>
        </div>
      )}

      {calculated && !loading && results.length > 0 && (
        <>
          <div className="bg-white shadow rounded-lg p-6 mb-6">
            <h2 className="text-lg font-semibold mb-4">Summary</h2>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Total Accounts Processed
                </label>
                <p className="text-2xl font-bold text-gray-900">{results.length}</p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Total Interest Calculated
                </label>
                <p className="text-2xl font-bold text-green-600">
                  {formatCurrency(totalInterest.toString())}
                </p>
              </div>
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Processing Date
                </label>
                <p className="text-2xl font-bold text-gray-900">
                  {new Date(processingDate).toLocaleDateString('en-US', {
                    year: 'numeric',
                    month: 'short',
                    day: 'numeric',
                  })}
                </p>
              </div>
            </div>
          </div>

          <div className="bg-white shadow rounded-lg overflow-hidden">
            <div className="px-6 py-4 border-b border-gray-200">
              <h2 className="text-lg font-semibold">Interest Calculation Results</h2>
            </div>
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Account ID
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Balance
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Interest Rate
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Monthly Interest
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Transaction Type
                    </th>
                    <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Category
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {results.map((result, index) => (
                    <tr key={`${result.accountId}-${index}`} className="hover:bg-gray-50">
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                        {result.accountId}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {formatCurrency(result.balance)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                        {formatPercentage(result.interestRate)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm font-semibold text-green-600">
                        {formatCurrency(result.monthlyInterest)}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {result.transactionTypeCode}
                      </td>
                      <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                        {result.transactionCategoryCode}
                      </td>
                    </tr>
                  ))}
                </tbody>
                <tfoot className="bg-gray-50">
                  <tr>
                    <td colSpan={3} className="px-6 py-4 text-sm font-semibold text-gray-900 text-right">
                      Total Interest:
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-bold text-green-600">
                      {formatCurrency(totalInterest.toString())}
                    </td>
                    <td colSpan={2}></td>
                  </tr>
                </tfoot>
              </table>
            </div>
          </div>

          <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-sm text-blue-800">
              <strong>Note:</strong> Interest has been calculated using the formula: (Balance × Interest Rate) ÷ 1200.
              The results show the monthly interest charges that will be applied to each account.
            </p>
          </div>
        </>
      )}
    </div>
  );
}
