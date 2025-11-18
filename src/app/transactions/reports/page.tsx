'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { TransactionReportRequest } from '@/types/transaction';
import { Input, Button } from '@/components/ui';

export default function TransactionReportPage() {
  const router = useRouter();
  const [reportType, setReportType] = useState<'MONTHLY' | 'YEARLY' | 'CUSTOM'>('MONTHLY');
  const [startDate, setStartDate] = useState({
    month: '',
    day: '',
    year: '',
  });
  const [endDate, setEndDate] = useState({
    month: '',
    day: '',
    year: '',
  });
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [validationErrors, setValidationErrors] = useState<Record<string, string>>({});
  const [successMessage, setSuccessMessage] = useState<string | null>(null);

  const validateCustomDates = (): boolean => {
    if (reportType !== 'CUSTOM') {
      return true;
    }

    const errors: Record<string, string> = {};

    if (!startDate.month || !startDate.day || !startDate.year) {
      errors.startDate = 'Start date is required for custom reports';
    } else {
      const month = parseInt(startDate.month);
      const day = parseInt(startDate.day);
      const year = parseInt(startDate.year);

      if (month < 1 || month > 12) {
        errors.startDate = 'Start month must be between 1 and 12';
      }
      if (day < 1 || day > 31) {
        errors.startDate = 'Start day must be between 1 and 31';
      }
      if (year < 1900 || year > 2100) {
        errors.startDate = 'Start year must be between 1900 and 2100';
      }
    }

    if (!endDate.month || !endDate.day || !endDate.year) {
      errors.endDate = 'End date is required for custom reports';
    } else {
      const month = parseInt(endDate.month);
      const day = parseInt(endDate.day);
      const year = parseInt(endDate.year);

      if (month < 1 || month > 12) {
        errors.endDate = 'End month must be between 1 and 12';
      }
      if (day < 1 || day > 31) {
        errors.endDate = 'End day must be between 1 and 31';
      }
      if (year < 1900 || year > 2100) {
        errors.endDate = 'End year must be between 1900 and 2100';
      }
    }

    if (!errors.startDate && !errors.endDate) {
      const start = new Date(
        parseInt(startDate.year),
        parseInt(startDate.month) - 1,
        parseInt(startDate.day)
      );
      const end = new Date(
        parseInt(endDate.year),
        parseInt(endDate.month) - 1,
        parseInt(endDate.day)
      );

      if (start > end) {
        errors.endDate = 'End date must be after start date';
      }
    }

    setValidationErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateCustomDates()) {
      return;
    }

    try {
      setLoading(true);
      setError(null);
      setSuccessMessage(null);

      const reportRequest: TransactionReportRequest = {
        reportType,
      };

      if (reportType === 'CUSTOM') {
        reportRequest.startDate = `${startDate.year}-${startDate.month.padStart(2, '0')}-${startDate.day.padStart(2, '0')}`;
        reportRequest.endDate = `${endDate.year}-${endDate.month.padStart(2, '0')}-${endDate.day.padStart(2, '0')}`;
      }

      await new Promise(resolve => setTimeout(resolve, 1000));

      setSuccessMessage(`${reportType} report has been submitted successfully. Report will be generated and available shortly.`);
      
      setTimeout(() => {
        handleClear();
      }, 3000);
    } catch (err: any) {
      setError(err.message || 'Failed to submit report request');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  const handleClear = () => {
    setReportType('MONTHLY');
    setStartDate({ month: '', day: '', year: '' });
    setEndDate({ month: '', day: '', year: '' });
    setValidationErrors({});
    setError(null);
    setSuccessMessage(null);
  };

  return (
    <div className="p-6 max-w-3xl">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Transaction Report Generation</h1>
      </div>

      {error && (
        <div className="mb-4 p-4 bg-red-50 border border-red-200 text-red-700 rounded">
          {error}
        </div>
      )}

      {successMessage && (
        <div className="mb-4 p-4 bg-green-50 border border-green-200 text-green-700 rounded">
          {successMessage}
        </div>
      )}
      
      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="bg-white shadow rounded-lg p-6">
          <h2 className="text-lg font-semibold mb-4">Report Type Selection</h2>
          
          <div className="space-y-3">
            <label className="flex items-center space-x-3 cursor-pointer">
              <input
                type="radio"
                name="reportType"
                value="MONTHLY"
                checked={reportType === 'MONTHLY'}
                onChange={(e) => setReportType(e.target.value as 'MONTHLY')}
                className="w-4 h-4 text-blue-600"
              />
              <span className="text-gray-900">Monthly Report</span>
            </label>
            
            <label className="flex items-center space-x-3 cursor-pointer">
              <input
                type="radio"
                name="reportType"
                value="YEARLY"
                checked={reportType === 'YEARLY'}
                onChange={(e) => setReportType(e.target.value as 'YEARLY')}
                className="w-4 h-4 text-blue-600"
              />
              <span className="text-gray-900">Yearly Report</span>
            </label>
            
            <label className="flex items-center space-x-3 cursor-pointer">
              <input
                type="radio"
                name="reportType"
                value="CUSTOM"
                checked={reportType === 'CUSTOM'}
                onChange={(e) => setReportType(e.target.value as 'CUSTOM')}
                className="w-4 h-4 text-blue-600"
              />
              <span className="text-gray-900">Custom Date Range</span>
            </label>
          </div>
        </div>

        {reportType === 'CUSTOM' && (
          <div className="bg-white shadow rounded-lg p-6">
            <h2 className="text-lg font-semibold mb-4">Custom Date Range</h2>
            
            <div className="space-y-4">
              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-2">
                  Start Date
                </label>
                <div className="grid grid-cols-3 gap-2">
                  <Input
                    placeholder="MM"
                    value={startDate.month}
                    onChange={(e) => setStartDate({ ...startDate, month: e.target.value })}
                    maxLength={2}
                    required
                  />
                  <Input
                    placeholder="DD"
                    value={startDate.day}
                    onChange={(e) => setStartDate({ ...startDate, day: e.target.value })}
                    maxLength={2}
                    required
                  />
                  <Input
                    placeholder="YYYY"
                    value={startDate.year}
                    onChange={(e) => setStartDate({ ...startDate, year: e.target.value })}
                    maxLength={4}
                    required
                  />
                </div>
                {validationErrors.startDate && (
                  <p className="mt-1 text-sm text-red-600">{validationErrors.startDate}</p>
                )}
              </div>

              <div>
                <label className="block text-sm font-semibold text-gray-700 mb-2">
                  End Date
                </label>
                <div className="grid grid-cols-3 gap-2">
                  <Input
                    placeholder="MM"
                    value={endDate.month}
                    onChange={(e) => setEndDate({ ...endDate, month: e.target.value })}
                    maxLength={2}
                    required
                  />
                  <Input
                    placeholder="DD"
                    value={endDate.day}
                    onChange={(e) => setEndDate({ ...endDate, day: e.target.value })}
                    maxLength={2}
                    required
                  />
                  <Input
                    placeholder="YYYY"
                    value={endDate.year}
                    onChange={(e) => setEndDate({ ...endDate, year: e.target.value })}
                    maxLength={4}
                    required
                  />
                </div>
                {validationErrors.endDate && (
                  <p className="mt-1 text-sm text-red-600">{validationErrors.endDate}</p>
                )}
              </div>
            </div>
          </div>
        )}

        {reportType !== 'CUSTOM' && (
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-sm text-blue-800">
              {reportType === 'MONTHLY' 
                ? 'The monthly report will include all transactions from the current month.'
                : 'The yearly report will include all transactions from the current year.'}
            </p>
          </div>
        )}
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Submitting...' : 'Generate Report'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={handleClear}
            disabled={loading}
          >
            Clear
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/transactions')}
          >
            Back to Transactions
          </Button>
        </div>
      </form>
    </div>
  );
}
