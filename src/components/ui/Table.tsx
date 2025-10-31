import { ReactNode } from 'react';

interface Column {
  key: string;
  label: string;
}

interface TableProps {
  children?: ReactNode;
  columns?: Column[];
  data?: Record<string, ReactNode>[];
  emptyMessage?: string;
}

interface TableHeaderProps {
  children: ReactNode;
}

interface TableBodyProps {
  children: ReactNode;
}

interface TableRowProps {
  children: ReactNode;
  className?: string;
}

interface TableCellProps {
  children: ReactNode;
  className?: string;
  colSpan?: number;
}

interface TableHeadProps {
  children: ReactNode;
  className?: string;
}

export function Table({ children, columns, data, emptyMessage = 'No data available' }: TableProps) {
  // If columns and data are provided, render data-driven table
  if (columns && data) {
    return (
      <div className="w-full overflow-auto">
        <table className="w-full caption-bottom text-sm">
          <TableHeader>
            <TableRow>
              {columns.map((column) => (
                <TableHead key={column.key}>{column.label}</TableHead>
              ))}
            </TableRow>
          </TableHeader>
          <TableBody>
            {data.length === 0 ? (
              <TableRow>
                <TableCell colSpan={columns.length} className="text-center text-muted-foreground py-8">
                  {emptyMessage}
                </TableCell>
              </TableRow>
            ) : (
              data.map((row, rowIndex) => (
                <TableRow key={rowIndex}>
                  {columns.map((column) => (
                    <TableCell key={column.key}>
                      {row[column.key]}
                    </TableCell>
                  ))}
                </TableRow>
              ))
            )}
          </TableBody>
        </table>
      </div>
    );
  }

  // Otherwise, render compositional table
  return (
    <div className="w-full overflow-auto">
      <table className="w-full caption-bottom text-sm">
        {children}
      </table>
    </div>
  );
}

export function TableHeader({ children }: TableHeaderProps) {
  return <thead className="[&_tr]:border-b">{children}</thead>;
}

export function TableBody({ children }: TableBodyProps) {
  return <tbody className="[&_tr:last-child]:border-0">{children}</tbody>;
}

export function TableRow({ children, className = '' }: TableRowProps) {
  return (
    <tr className={`border-b border-border transition-colors hover:bg-muted/50 data-[state=selected]:bg-muted ${className}`}>
      {children}
    </tr>
  );
}

export function TableHead({ children, className = '' }: TableHeadProps) {
  return (
    <th className={`h-12 px-4 text-left align-middle font-medium text-muted-foreground [&:has([role=checkbox])]:pr-0 ${className}`}>
      {children}
    </th>
  );
}

export function TableCell({ children, className = '', colSpan }: TableCellProps) {
  return (
    <td className={`p-4 align-middle [&:has([role=checkbox])]:pr-0 ${className}`} colSpan={colSpan}>
      {children}
    </td>
  );
}