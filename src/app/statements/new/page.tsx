'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { statementService } from '@/services/statementService';
import { AccountStatement, InterestCalculation } from '@/types/account';

const NewStatementPage: React.FC = () => {
  const router = useRouter();
  const [generating, setGenerating] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<string | null>(null);
  const [generationType, setGenerationType] = useState<'account' | 'card'>('account');
  const [identifier, setIdentifier] = useState('');
  const [calculatingInterest, setCalculatingInterest] = useState(false);
  const [interestResults, setInterestResults] = useState<InterestCalculation[] | null>(null);
  const [processingDate, setProcessingDate] = useState(new Date().toISOString().split('T')[0]);

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const { name, value } = e.target;
    
    // Clear field error when user starts typing
    if (fieldErrors[name]) {
      setFieldErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }

    if (name === 'identifier') {
      setIdentifier(value);
    } else if (name === 'processingDate') {
      setProcessingDate(value);
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (!identifier.trim()) {
      errors.identifier = `${generationType === 'account' ? 'Account ID' : 'Card Number'} is required`;
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleGenerateStatement = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the errors in the form');
      return;
    }

    try {
      setGenerating(true);
      setError(null);
      setSuccess(null);

      let statement: AccountStatement | AccountStatement[];

      if (generationType === 'account') {
        statement = await statementService.getStatementsByAccountId(identifier);
      } else {
        statement = await statementService.getStatementByCardNumber(identifier);
      }

      setSuccess('Statement generated successfully!');
      
      // Redirect to view statement after a short delay
      setTimeout(() => {
        if (generationType === 'account' && Array.isArray(statement) && statement.length > 0) {
          router.push(`/statements/${statement[0].account.accountId}`);
        } else if (!Array.isArray(statement)) {
          router.push(`/statements/${statement.account.accountId}`);
        } else {
          router.push('/statements');
        }
      }, 1500);
    } catch (err) {
      console.error('Error generating statement:', err);
      setError(err instanceof Error ? err.message : 'Failed to generate statement');
      setGenerating(false);
    }
  };

  const handleCalculateInterest = async () => {
    try {
      setCalculatingInterest(true);
      setError(null);
      setInterestResults(null);

      const results = await statementService.calculateInterest(processingDate);
      setInterestResults(results);
      setSuccess(`Interest calculated successfully for ${results.length} account(s)!`);
    } catch (err) {
      console.error('Error calculating interest:', err);
      setError(err instanceof Error ? err.message : 'Failed to calculate interest');
    } finally {
      setCalculatingInterest(false);
    }
  };

  const handleCancel = () => {
    router.push('/statements');
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
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Generate Statement & Calculate Interest</h1>
        <p className="text-gray-600">Generate account statements or calculate interest for all accounts</p>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Generate Statement Section */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Generate Statement</h2>
          </div>
          
          <form onSubmit={handleGenerateStatement} className="p-6">
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

            {success && (
              <div className="mb-6 bg-green-50 border border-green-200 text-green-800 px-4 py-3 rounded-md">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-green-400" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm font-medium">{success}</p>
                  </div>
                </div>
              </div>
            )}

            <div className="space-y-6">
              {/* Generation Type */}
              <div>
                <label className="block text-sm font-medium text-gray-700 mb-2">
                  Generate Statement By <span className="text-red-500">*</span>
                </label>
                <div className="flex gap-4">
                  <label className="flex items-center">
                    <input
                      type="radio"
                      name="generationType"
                      value="account"
                      checked={generationType === 'account'}
                      onChange={(e) => {
                        setGenerationType(e.target.value as 'account' | 'card');
                        setIdentifier('');
                        setFieldErrors({});
                      }}
                      className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                      disabled={generating}
                    />
                    <span className="ml-2 text-sm text-gray-700">Account ID</span>
                  </label>
                  <label className="flex items-center">
                    <input
                      type="radio"
                      name="generationType"
                      value="card"
                      checked={generationType === 'card'}
                      onChange={(e) => {
                        setGenerationType(e.target.value as 'account' | 'card');
                        setIdentifier('');
                        setFieldErrors({});
                      }}
                      className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300"
                      disabled={generating}
                    />
                    <span className="ml-2 text-sm text-gray-700">Card Number</span>
                  </label>
                </div>
              </div>

              {/* Identifier Input */}
              <div>
                <label htmlFor="identifier" className="block text-sm font-medium text-gray-700 mb-1">
                  {generationType === 'account' ? 'Account ID' : 'Card Number'} <span className="text-red-500">*</span>
                </label>
                <Input
                  id="identifier"
                  name="identifier"
                  type="text"
                  value={identifier}
                  onChange={handleInputChange}
                  placeholder={`Enter ${generationType === 'account' ? 'account ID' : 'card number'}`}
                  className={fieldErrors.identifier ? 'border-red-500' : ''}
                  disabled={generating}
                />
                {fieldErrors.identifier && (
                  <p className="mt-1 text-sm text-red-600">{fieldErrors.identifier}</p>
                )}
              </div>

              {/* Information Box */}
              <div className="bg-blue-50 border border-blue-200 rounded-md p-4">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium text-blue-800">Statement Information</h3>
                    <div className="mt-2 text-sm text-blue-700">
                      <p>The statement will include:</p>
                      <ul className="list-disc list-inside mt-1 space-y-1">
                        <li>Account details and current balance</li>
                        <li>Customer information</li>
                        <li>All transactions for the account</li>
                        <li>Total transaction amount</li>
                      </ul>
                    </div>
                  </div>
                </div>
              </div>

              {/* Form Actions */}
              <div className="flex justify-end gap-3 pt-4 border-t border-gray-200">
                <Button
                  type="button"
                  variant="secondary"
                  onClick={handleCancel}
                  disabled={generating}
                >
                  Cancel
                </Button>
                <Button
                  type="submit"
                  disabled={generating}
                >
                  {generating ? 'Generating...' : 'Generate Statement'}
                </Button>
              </div>
            </div>
          </form>
        </div>

        {/* Calculate Interest Section */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Calculate Interest</h2>
          </div>
          
          <div className="p-6">
            <div className="space-y-6">
              {/* Processing Date */}
              <div>
                <label htmlFor="processingDate" className="block text-sm font-medium text-gray-700 mb-1">
                  Processing Date
                </label>
                <Input
                  id="processingDate"
                  name="processingDate"
                  type="date"
                  value={processingDate}
                  onChange={handleInputChange}
                  disabled={calculatingInterest}
                />
                <p className="mt-1 text-xs text-gray-500">
                  Leave as today&apos;s date or select a specific processing date
                </p>
              </div>

              {/* Information Box */}
              <div className="bg-yellow-50 border border-yellow-200 rounded-md p-4">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-5 w-5 text-yellow-400" viewBox="0 0 20 20" fill="currentColor">
                      <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium text-yellow-800">Interest Calculation</h3>
                    <div className="mt-2 text-sm text-yellow-700">
                      <p>This will calculate interest for all accounts based on:</p>
                      <ul className="list-disc list-inside mt-1 space-y-1">
                        <li>Current account balances</li>
                        <li>Applicable interest rates</li>
                        <li>The specified processing date</li>
                      </ul>
                    </div>
                  </div>
                </div>
              </div>

              {/* Calculate Button */}
              <Button
                type="button"
                onClick={handleCalculateInterest}
                disabled={calculatingInterest}
                className="w-full"
              >
                {calculatingInterest ? 'Calculating...' : 'Calculate Interest for All Accounts'}
              </Button>

              {/* Interest Results */}
              {interestResults && interestResults.length > 0 && (
                <div className="mt-6 border border-gray-200 rounded-md overflow-hidden">
                  <div className="bg-gray-50 px-4 py-3 border-b border-gray-200">
                    <h3 className="text-sm font-semibold text-gray-900">Interest Calculation Results</h3>
                  </div>
                  <div className="max-h-96 overflow-y-auto">
                    <table className="min-w-full divide-y divide-gray-200">
                      <thead className="bg-gray-50">
                        <tr>
                          <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                            Account ID
                          </th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                            Balance
                          </th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                            Rate
                          </th>
                          <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase">
                            Interest
                          </th>
                        </tr>
                      </thead>
                      <tbody className="bg-white divide-y divide-gray-200">
                        {interestResults.map((result, index) => (
                          <tr key={index} className="hover:bg-gray-50">
                            <td className="px-4 py-3 text-sm font-medium text-gray-900">
                              {result.accountId}
                            </td>
                            <td className="px-4 py-3 text-sm text-gray-900">
                              {formatCurrency(result.balance)}
                            </td>
                            <td className="px-4 py-3 text-sm text-gray-900">
                              {(result.interestRate * 100).toFixed(2)}%
                            </td>
                            <td className="px-4 py-3 text-sm font-medium text-green-600">
                              {formatCurrency(result.monthlyInterest)}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                  <div className="bg-gray-50 px-4 py-3 border-t border-gray-200">
                    <p className="text-sm text-gray-700">
                      Total Interest: <span className="font-medium text-green-600">
                        {formatCurrency(interestResults.reduce((sum, r) => sum + r.monthlyInterest, 0))}
                      </span>
                    </p>
                  </div>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default NewStatementPage;
