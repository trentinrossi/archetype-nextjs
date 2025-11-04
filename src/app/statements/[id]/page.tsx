'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter, useSearchParams } from 'next/navigation';
import { Button } from '@/components/ui';
import { statementService } from '@/services/statementService';
import { AccountStatement } from '@/types/account';

const StatementDetailPage: React.FC = () => {
  const params = useParams();
  const searchParams = useSearchParams();
  const router = useRouter();
  
  // The ID could be either account ID or card number
  const id = params.id as string;
  const type = searchParams.get('type') || 'account'; // 'account' or 'card'
  
  const [statement, setStatement] = useState<AccountStatement | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchStatement = async () => {
      try {
        setLoading(true);
        setError(null);
        
        if (type === 'card') {
          const data = await statementService.getStatementByCardNumber(id);
          setStatement(data);
        } else {
          // For account, get the first statement (or you could show a list)
          const data = await statementService.getStatementsByAccountId(id);
          if (data && data.length > 0) {
            setStatement(data[0]);
          } else {
            setError('No statement found for this account');
          }
        }
      } catch (err) {
        console.error('Error fetching statement:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch statement');
      } finally {
        setLoading(false);
      }
    };

    if (id) {
      fetchStatement();
    }
  }, [id, type]);

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'long',
      day: 'numeric',
    });
  };

  const formatDateTime = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
    });
  };

  const handlePrint = () => {
    window.print();
  };

  const handleDownloadPDF = () => {
    // Placeholder for PDF download functionality
    alert('PDF download functionality would be implemented here');
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading statement...</p>
        </div>
      </div>
    );
  }

  if (error || !statement) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Statement not found'}</p>
          <Button onClick={() => router.push('/statements')}>Back to Statements</Button>
        </div>
      </div>
    );
  }

  const { customer, account, transactions, totalAmount } = statement;

  return (
    <div className="container mx-auto px-4 py-8">
      {/* Header with Actions */}
      <div className="mb-8 print:hidden">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">Account Statement</h1>
            <p className="text-gray-600">Account: {account.accountId}</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={() => router.push('/statements')}>
              Back to List
            </Button>
            <Button variant="secondary" onClick={handlePrint}>
              Print
            </Button>
            <Button variant="secondary" onClick={handleDownloadPDF}>
              Download PDF
            </Button>
          </div>
        </div>
      </div>

      {/* Statement Document */}
      <div className="bg-white shadow-lg rounded-lg overflow-hidden">
        {/* Statement Header */}
        <div className="bg-gradient-to-r from-blue-600 to-blue-800 text-white p-8">
          <div className="flex justify-between items-start">
            <div>
              <h2 className="text-3xl font-bold mb-2">ACCOUNT STATEMENT</h2>
              <p className="text-blue-100">Statement Period: {formatDate(account.openDate)}</p>
            </div>
            <div className="text-right">
              <div className="text-sm text-blue-100">Account Number</div>
              <div className="text-2xl font-bold">{account.accountId}</div>
            </div>
          </div>
        </div>

        {/* Customer & Account Information */}
        <div className="p-8 border-b border-gray-200">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {/* Customer Information */}
            <div>
              <h3 className="text-lg font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
                Customer Information
              </h3>
              <div className="space-y-3 text-sm">
                <div>
                  <span className="text-gray-600">Name:</span>
                  <span className="ml-2 font-medium text-gray-900">
                    {customer.firstName} {customer.middleName} {customer.lastName}
                  </span>
                </div>
                <div>
                  <span className="text-gray-600">Customer ID:</span>
                  <span className="ml-2 font-medium text-gray-900">{customer.customerId}</span>
                </div>
                <div>
                  <span className="text-gray-600">Address:</span>
                  <div className="ml-2 mt-1 text-gray-900">
                    <div>{customer.addressLine1}</div>
                    {customer.addressLine2 && <div>{customer.addressLine2}</div>}
                    {customer.addressLine3 && <div>{customer.addressLine3}</div>}
                    <div>{customer.stateCode}, {customer.countryCode} {customer.zipCode}</div>
                  </div>
                </div>
                <div>
                  <span className="text-gray-600">Phone:</span>
                  <span className="ml-2 font-medium text-gray-900">{customer.phoneNumber1}</span>
                </div>
              </div>
            </div>

            {/* Account Summary */}
            <div>
              <h3 className="text-lg font-semibold text-gray-900 mb-4 pb-2 border-b border-gray-200">
                Account Summary
              </h3>
              <div className="space-y-3">
                <div className="flex justify-between items-center py-2 border-b border-gray-100">
                  <span className="text-sm text-gray-600">Previous Balance</span>
                  <span className="text-sm font-medium text-gray-900">
                    {formatCurrency(account.currentBalance - totalAmount)}
                  </span>
                </div>
                <div className="flex justify-between items-center py-2 border-b border-gray-100">
                  <span className="text-sm text-gray-600">Payments & Credits</span>
                  <span className="text-sm font-medium text-green-600">
                    {formatCurrency(account.currentCycleCredit)}
                  </span>
                </div>
                <div className="flex justify-between items-center py-2 border-b border-gray-100">
                  <span className="text-sm text-gray-600">Purchases & Debits</span>
                  <span className="text-sm font-medium text-red-600">
                    {formatCurrency(account.currentCycleDebit)}
                  </span>
                </div>
                <div className="flex justify-between items-center py-3 bg-blue-50 px-3 rounded-lg mt-4">
                  <span className="text-base font-semibold text-gray-900">Current Balance</span>
                  <span className="text-xl font-bold text-blue-600">
                    {formatCurrency(account.currentBalance)}
                  </span>
                </div>
                <div className="flex justify-between items-center py-2">
                  <span className="text-sm text-gray-600">Credit Limit</span>
                  <span className="text-sm font-medium text-gray-900">
                    {formatCurrency(account.creditLimit)}
                  </span>
                </div>
                <div className="flex justify-between items-center py-2">
                  <span className="text-sm text-gray-600">Available Credit</span>
                  <span className="text-sm font-medium text-gray-900">
                    {formatCurrency(account.creditLimit - account.currentBalance)}
                  </span>
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* Transaction History */}
        <div className="p-8">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Transaction History</h3>
          
          {transactions.length === 0 ? (
            <div className="text-center py-8 bg-gray-50 rounded-lg">
              <p className="text-gray-600">No transactions in this statement period</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <table className="min-w-full divide-y divide-gray-200">
                <thead className="bg-gray-50">
                  <tr>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Date
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Description
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Merchant
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Type
                    </th>
                    <th className="px-4 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                      Amount
                    </th>
                  </tr>
                </thead>
                <tbody className="bg-white divide-y divide-gray-200">
                  {transactions.map((transaction) => (
                    <tr key={transaction.transactionId} className="hover:bg-gray-50">
                      <td className="px-4 py-3 whitespace-nowrap text-sm text-gray-900">
                        {formatDateTime(transaction.originalTimestamp)}
                      </td>
                      <td className="px-4 py-3 text-sm text-gray-900">
                        {transaction.transactionDescription}
                      </td>
                      <td className="px-4 py-3 text-sm text-gray-900">
                        {transaction.merchantName}
                        {transaction.merchantCity && (
                          <div className="text-xs text-gray-500">{transaction.merchantCity}</div>
                        )}
                      </td>
                      <td className="px-4 py-3 whitespace-nowrap text-sm">
                        <span className="inline-flex items-center px-2 py-1 rounded text-xs font-medium bg-gray-100 text-gray-800">
                          {transaction.transactionTypeCode}
                        </span>
                      </td>
                      <td className={`px-4 py-3 whitespace-nowrap text-sm text-right font-medium ${
                        transaction.transactionAmount < 0 ? 'text-green-600' : 'text-gray-900'
                      }`}>
                        {formatCurrency(Math.abs(transaction.transactionAmount))}
                        {transaction.transactionAmount < 0 && ' CR'}
                      </td>
                    </tr>
                  ))}
                </tbody>
                <tfoot className="bg-gray-50">
                  <tr>
                    <td colSpan={4} className="px-4 py-4 text-right text-sm font-semibold text-gray-900">
                      Total Transaction Amount:
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-right font-bold text-gray-900">
                      {formatCurrency(totalAmount)}
                    </td>
                  </tr>
                </tfoot>
              </table>
            </div>
          )}
        </div>

        {/* Important Information */}
        <div className="p-8 bg-gray-50 border-t border-gray-200">
          <h3 className="text-sm font-semibold text-gray-900 mb-3">Important Information</h3>
          <div className="text-xs text-gray-600 space-y-2">
            <p>• Payment is due by the end of each billing cycle to avoid late fees.</p>
            <p>• Minimum payment required: {formatCurrency(account.currentBalance * 0.02)}</p>
            <p>• Account expiration date: {formatDate(account.expirationDate)}</p>
            <p>• For questions about your statement, please contact customer service.</p>
            <p>• Keep this statement for your records.</p>
          </div>
        </div>

        {/* Footer */}
        <div className="p-6 bg-gray-100 border-t border-gray-200 text-center">
          <p className="text-xs text-gray-600">
            This statement is generated electronically. For support, please contact our customer service team.
          </p>
          <p className="text-xs text-gray-500 mt-2">
            Statement generated on {new Date().toLocaleDateString('en-US', {
              year: 'numeric',
              month: 'long',
              day: 'numeric',
            })}
          </p>
        </div>
      </div>
    </div>
  );
};

export default StatementDetailPage;
