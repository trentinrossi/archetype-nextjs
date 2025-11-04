'use client';

import React, { useState } from 'react';
import { useParams, useRouter, useSearchParams } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { statementService } from '@/services/statementService';
import { InterestCalculation } from '@/types/account';

const EditStatementPage: React.FC = () => {
  const params = useParams();
  const searchParams = useSearchParams();
  const router = useRouter();
  
  const id = params.id as string;
  const type = searchParams.get('type') || 'account';
  
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [successMessage, setSuccessMessage] = useState<string | null>(null);
  
  const [formData, setFormData] = useState({
    processingDate: new Date().toISOString().split('T')[0],
    includeInterest: false,
  });

  const [interestResults, setInterestResults] = useState<InterestCalculation[]>([]);
  const [showInterestResults, setShowInterestResults] = useState(false);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value, type, checked } = e.target;
    
    setFormData(prev => ({
      ...prev,
      [name]: type === 'checkbox' ? checked : value,
    }));
  };

  const handleCalculateInterest = async () => {
    try {
      setLoading(true);
      setError(null);
      const results = await statementService.calculateInterest(formData.processingDate);
      setInterestResults(results);
      setShowInterestResults(true);
      setSuccessMessage('Interest calculated successfully');
    } catch (err) {
      console.error('Error calculating interest:', err);
      setError(err instanceof Error ? err.message : 'Failed to calculate interest');
    } finally {
      setLoading(false);
    }
  };

  const handleRegenerateStatement = async () => {
    try {
      setLoading(true);
      setError(null);
      
      // Placeholder for statement regeneration
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      setSuccessMessage('Statement regeneration initiated. This may take a few moments.');
      
      // Redirect back to view after a short delay
      setTimeout(() => {
        router.push(`/statements/${id}?type=${type}`);
      }, 2000);
    } catch (err) {
      console.error('Error regenerating statement:', err);
      setError(err instanceof Error ? err.message : 'Failed to regenerate statement');
      setLoading(false);
    }
  };

  const handleCancel = () => {
    router.push(`/statements/${id}?type=${type}`);
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Statement Settings</h1>
        <p className="text-gray-600">
          {type === 'card' ? `Card Number: ${id}` : `Account ID: ${id}`}
        </p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Main Settings */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Statement Parameters</h2>
          </div>
          <div className="p-6">
            {error && (
              <div className="mb-6 bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded-md">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm font-medium">{error}</p>
                  </div>
                </div>
              </div>
            )}

            {successMessage && (
              <div className="mb-6 bg-green-50 border border-green-200 text-green-800 px-4 py-3 rounded-md">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-green-400" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm font-medium">{successMessage}</p>
                  </div>
                </div>
              </div>
            )}

            <div className="space-y-6">
              <div>
                <label htmlFor="processingDate" className="block text-sm font-medium text-gray-700 mb-1">
                  Processing Date
                </label>
                <Input
                  id="processingDate"
                  name="processingDate"
                  type="date"
                  value={formData.processingDate}
                  onChange={handleInputChange}
                  disabled={loading}
                />
                <p className="mt-1 text-xs text-gray-500">Select the date for statement processing</p>
              </div>

              <div className="flex items-center">
                <input
                  id="includeInterest"
                  name="includeInterest"
                  type="checkbox"
                  checked={formData.includeInterest}
                  onChange={handleInputChange}
                  disabled={loading}
                  className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
                />
                <label htmlFor="includeInterest" className="ml-2 block text-sm text-gray-900">
                  Include interest calculation in statement
                </label>
              </div>
            </div>

            <div className="mt-8 pt-6 border-t border-gray-200">
              <h3 className="text-sm font-semibold text-gray-900 mb-4">Actions</h3>
              <div className="space-y-3">
                <Button
                  onClick={handleCalculateInterest}
                  disabled={loading}
                  className="w-full"
                  variant="secondary"
                >
                  {loading ? 'Calculating...' : 'Calculate Interest'}
                </Button>
                <Button
                  onClick={handleRegenerateStatement}
                  disabled={loading}
                  className="w-full"
                >
                  {loading ? 'Processing...' : 'Regenerate Statement'}
                </Button>
              </div>
            </div>

            <div className="mt-6 flex justify-end gap-3">
              <Button
                type="button"
                variant="secondary"
                onClick={handleCancel}
                disabled={loading}
              >
                Cancel
              </Button>
            </div>
          </div>
        </div>

        {/* Interest Results */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Interest Calculation Results</h2>
          </div>
          <div className="p-6">
            {!showInterestResults ? (
              <div className="text-center py-12">
                <svg className="mx-auto h-12 w-12 text-gray-400" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                  <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 7h6m0 10v-3m-3 3h.01M9 17h.01M9 14h.01M12 14h.01M15 11h.01M12 11h.01M9 11h.01M7 21h10a2 2 0 002-2V5a2 2 0 00-2-2H7a2 2 0 00-2 2v14a2 2 0 002 2z" />
                </svg>
                <p className="mt-4 text-sm text-gray-600">Click &ldquo;Calculate Interest&rdquo; to see results</p>
              </div>
            ) : (
              <div className="space-y-4">
                {interestResults.length === 0 ? (
                  <p className="text-sm text-gray-600">No interest calculations available</p>
                ) : (
                  interestResults.map((result, index) => (
                    <div key={index} className="border border-gray-200 rounded-lg p-4">
                      <div className="flex justify-between items-start mb-3">
                        <div>
                          <div className="text-sm font-medium text-gray-900">Account: {result.accountId}</div>
                          <div className="text-xs text-gray-500 mt-1">
                            {result.transactionTypeCode} - {result.transactionCategoryCode}
                          </div>
                        </div>
                        <div className="text-right">
                          <div className="text-lg font-bold text-blue-600">
                            {formatCurrency(result.monthlyInterest)}
                          </div>
                          <div className="text-xs text-gray-500">Monthly Interest</div>
                        </div>
                      </div>
                      <div className="grid grid-cols-2 gap-4 text-sm">
                        <div>
                          <span className="text-gray-600">Balance:</span>
                          <span className="ml-2 font-medium text-gray-900">
                            {formatCurrency(result.balance)}
                          </span>
                        </div>
                        <div>
                          <span className="text-gray-600">Rate:</span>
                          <span className="ml-2 font-medium text-gray-900">
                            {(result.interestRate * 100).toFixed(2)}%
                          </span>
                        </div>
                      </div>
                    </div>
                  ))
                )}
                {interestResults.length > 0 && (
                  <div className="mt-4 pt-4 border-t border-gray-200">
                    <div className="flex justify-between items-center">
                      <span className="text-sm font-semibold text-gray-900">Total Interest:</span>
                      <span className="text-xl font-bold text-blue-600">
                        {formatCurrency(
                          interestResults.reduce((sum, r) => sum + r.monthlyInterest, 0)
                        )}
                      </span>
                    </div>
                  </div>
                )}
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Information Boxes */}
      <div className="mt-6 grid grid-cols-1 lg:grid-cols-2 gap-6">
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <h3 className="text-sm font-medium text-blue-800">About Statement Regeneration</h3>
              <div className="mt-2 text-sm text-blue-700">
                <p>Regenerating a statement will recalculate all transactions and balances for the specified period. This action may take several minutes to complete.</p>
              </div>
            </div>
          </div>
        </div>

        <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
          <div className="flex">
            <div className="flex-shrink-0">
              <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
              </svg>
            </div>
            <div className="ml-3">
              <h3 className="text-sm font-medium text-yellow-800">Important Notice</h3>
              <div className="mt-2 text-sm text-yellow-700">
                <p>Statement changes are logged for compliance. Ensure you have proper authorization before regenerating statements or modifying parameters.</p>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default EditStatementPage;
